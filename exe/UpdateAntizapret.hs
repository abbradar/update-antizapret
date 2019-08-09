import Data.Maybe
import Data.List
import Data.Char
import Data.Fixed
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import System.Environment
import Control.Concurrent (threadDelay, forkIO, throwTo, myThreadId)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSBuilder
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Text (Text)
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as JSON
import Data.IP
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Time.Clock
import Text.Feed.Import
import Text.Feed.Query
import Network.HTTP.Simple
import Control.Monad.Trans.Resource (MonadUnliftIO, runResourceT)
import Control.Monad.Catch
import qualified System.FSNotify as FSNotify
import System.FilePath
import qualified Codec.Text.IConv as IConv
import Control.Monad.Trans.Control
import Text.InterpolatedString.Perl6
import Network.Socket (HostName)
import Network.DNS.Resolver

import Paths_update_antizapret
import Antizapret.Types
import Control.Concurrent.STM.TEVar
import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet
import qualified Antizapret.Format.Simple as Format
import qualified Antizapret.Format.ZapretInfo as Format
import qualified Antizapret.Filter.Coarse as Coarse
import qualified Antizapret.Output.IPSet as Output
import qualified Antizapret.Output.PAC as Output
import qualified Antizapret.DNS as DNSCache

reservedRanges :: [AddrRange IPv4]
reservedRanges = [ "0.0.0.0/8"
                 , "10.0.0.0/8"
                 , "127.0.0.0/8"
                 , "224.0.0.0/4"
                 , "240.0.0.0/4"
                 , "172.16.0.0/12"
                 , "192.168.0.0/16"
                 ]

jsonOptions :: String -> JSON.Options
jsonOptions prefix = JSON.defaultOptions { JSON.fieldLabelModifier = JSON.camelTo2 '_' . fromJust . stripPrefix prefix
                                         , JSON.constructorTagModifier = \x -> JSON.camelTo2 '_' $ fromMaybe x $ stripPrefix prefixUp x
                                         , JSON.sumEncoding = JSON.defaultTaggedObject { JSON.tagFieldName = "type" }
                                         }
  where prefixUp = case prefix of
          ltr : str -> toUpper ltr : str
          "" -> ""

data SourceConfig = SourceFeed { sourceUrl :: String
                               , sourceInterval :: Int
                               , sourceDataUrl :: String
                               }
                  | SourceFilesystem { sourcePath :: FilePath
                                     }
                  deriving (Show, Eq, Generic)

instance FromJSON SourceConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "source"

data InputFormatConfig = Simple
                       | ZapretInfo
                       deriving (Show, Eq, Generic)

instance FromJSON InputFormatConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions ""

data InputConfig = InputConfig { inputSource :: SourceConfig
                               , inputFormat :: InputFormatConfig
                               }
                 deriving (Show, Eq, Generic)

instance FromJSON InputConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "input"

data FilterConfig = FilterCoarse { filterSpecificity :: Int
                                 }
                  deriving (Show, Eq, Generic)

instance FromJSON FilterConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "filter"

data SinkConfig = SinkFilesystem { sinkPath :: FilePath
                                 }
                | SinkNull
                deriving (Show, Eq, Generic)

instance FromJSON SinkConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "sink"

data OutputFormatConfig = IPSet
                        | PAC { pacProxy :: Text
                              }
                        deriving (Show, Eq, Generic)

instance FromJSON OutputFormatConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "pac"

data OutputConfig = OutputConfig { outputSink :: SinkConfig
                                 , outputFilters :: [FilterConfig]
                                 , outputFormat :: OutputFormatConfig
                                 }
                  deriving (Show, Eq, Generic)

instance FromJSON OutputConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "output"

data DNSConfig = DNSConfig { dnsServer :: HostName
                           , dnsThreads :: Int
                           , dnsInterval :: NominalDiffTime
                           }
               deriving (Show, Eq, Generic)

instance FromJSON DNSConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "dns"

data Config = Config { inputs :: [InputConfig]
                     , outputs :: [OutputConfig]
                     , dns :: DNSConfig
                     }
            deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = JSON.genericParseJSON $ jsonOptions ""

type MonadAZ m = (MonadLogger m, MonadBaseControl IO m, MonadIO m, MonadMask m, MonadUnliftIO m)

type AZSink = forall m1. (MonadLogger m1, MonadThrow m1, MonadIO m1) => ConduitT BS.ByteString Void m1 ()

expectFeed :: forall m. (MonadAZ m) => String -> Int -> String -> AZSink -> m ()
expectFeed url interval dataUrl sink = go Nothing
  where go :: Maybe Text -> m ()
        go oldTs = do
          ts <- do
            feed <- (parseFeedSource <$> getResponseBody <$> httpLBS (fromJust $ parseRequest url)) >>= \case
              Just feed -> return feed
              Nothing -> fail "expectFeed: invalid feed"
            let ts = getFeedLastUpdate feed
            when (isNothing ts) $ fail "expectFeed: no last update date"
            when (oldTs /= ts) $ runResourceT $ do
              httpSource (fromJust $ parseRequest dataUrl) getResponseBody `connect` sink
            return ts
            `catchAll` \e -> do
              $(logError) [qq|Failed to get update from URL: {e}|]
              return oldTs
          liftIO $ threadDelay (interval * 10^(6 :: Int))
          go ts

expectFilesystem :: MonadAZ m => FilePath -> AZSink -> m ()
expectFilesystem path sink = do
  manager <- liftIO $ FSNotify.startManager
  run
  $(logInfo) [qq|Watching for changes in directory {directory}, file {basename}|]
  _ <- liftBaseOpDiscard (FSNotify.watchDir manager directory checkEvent) $ \_ -> run
  return ()

  where checkEvent e = takeBaseName (FSNotify.eventPath e) == basename

        directory = takeDirectory path
        basename = takeBaseName path

        run = runResourceT $ do
          sourceFile path `connect` sink
          `catchAll` \e -> do
            $(logError) [qq|Failed to get update from file {path}: {e}|]

runInput :: forall m. MonadAZ m => InputConfig -> TEVar RawBlockList -> m ()
runInput (InputConfig {..}) resultVar = expect inputSink
  where reencodeZI :: Monad m1 => ConduitT BS.ByteString BS.ByteString m1 ()
        reencodeZI = do
          strings <- mconcat <$> map LBS.fromStrict <$> CL.consume
          yield $ LBS.toStrict $ IConv.convert "cp1251" "utf-8" strings

        formatConduit :: MonadThrow m1 => ConduitT BS.ByteString (PositionRange, RawBlockList) m1 ()
        formatConduit = case inputFormat of
          Simple -> CT.decodeUtf8 .| conduitParser Format.simple
          ZapretInfo -> reencodeZI .| CT.decodeUtf8 .| conduitParser Format.zapretInfo

        putResult :: (MonadLogger m1, MonadIO m1) => ConduitT (PositionRange, RawBlockList) Void m1 ()
        putResult = do
          !result <- maybe mempty snd <$> await
          $(logInfo) [qq|Source {inputSource} updated|]
          liftIO $ atomically $ writeTEVar resultVar result
        
        inputSink :: AZSink
        inputSink = formatConduit .| putResult
  
        expect :: AZSink -> m ()
        expect = case inputSource of
          SourceFeed {..} -> expectFeed sourceUrl sourceInterval sourceDataUrl
          SourceFilesystem {..} -> expectFilesystem sourcePath

applyFilter :: IPv4Set -> FilterConfig -> IPv4Set
applyFilter set (FilterCoarse {..}) = Coarse.coarseIPSet filterSpecificity set

writeOutput :: MonadAZ m => IPv4Set -> OutputConfig -> m ()
writeOutput set (OutputConfig {..}) = do
  let filtered = foldl' applyFilter set outputFilters
  rendered <- case outputFormat of
    IPSet -> return $ Output.toIPSetList filtered
    PAC {..} -> do
      templatePath <- liftIO $ getDataFileName "data/pac.template.js"
      template <- liftIO $ LBS.readFile templatePath
      return $ "var PROXY = " <> BSBuilder.lazyByteString (JSON.encode pacProxy) <> ";\n\n"
             <> Output.toPACGlobals filtered
             <> "\n" <> BSBuilder.lazyByteString template
  case outputSink of
    SinkFilesystem {..} -> liftIO $ withBinaryFile sinkPath WriteMode $ \h -> BSBuilder.hPutBuilder h rendered
    SinkNull -> return ()

main :: IO ()
main = do
  configPath <- getArgs >>= \case
    [configPath] -> return configPath
    _ -> fail "Usage: update-antizapret config.yaml"
  config <- Yaml.decodeFileEither configPath >>= \case
    Right config -> return config
    Left exception -> throwM exception

  unless (dnsThreads (dns config) > 0) $ fail "DNS threads count should be greater than zero"
  unless (dnsInterval (dns config) > 0) $ fail "DNS update interval should be positive"

  let dnsResolvConf = defaultResolvConf { resolvInfo = RCHostName $ dnsServer $ dns config }
  dnsResolvSeed <- makeResolvSeed dnsResolvConf

  tid <- myThreadId

  -- Sources polling
  sources <- forM (inputs config) $ \input -> do
    set <- newTEVarIO False mempty
    _ <- forkIO $ runStderrLoggingT (runInput input set) `catchAll` throwTo tid
    return set

  dnsSet <- newTEVarIO False mempty

  cache <- DNSCache.new
  dnsUpdateFlag <- newEmptyTMVarIO

  let requestDnsUpdate = liftIO $ void $ atomically $ tryPutTMVar dnsUpdateFlag ()
      -- Wait for a request and update DNS entries
      waitAndUpdateDns = forever $ do
        liftIO $ atomically $ takeTMVar dnsUpdateFlag
        $(logInfo) "Starting DNS entries refresh"
        changed <- liftIO $ newTVarIO False
        count <- liftIO $ newTVarIO (0 :: Int)

        let incrementCount = do
              currCount <- liftIO $ atomically $ do
                curr <- readTVar count
                let next = curr + 1
                writeTVar count next
                return next
              when (currCount > 0 && (currCount `mod` 10000 == 0)) $ 
                $(logInfo) [qq|Resolved {currCount} entries|]
            runUpdate resolver = do
              ret <- liftIO $ DNSCache.updateNext cache resolver
              case ret of
                Nothing -> return ()
                Just (Left (domain, e)) -> do
                  $(logError) [qq|DNS resolve error for domain {domain}: {e}|]
                  incrementCount
                  runUpdate resolver
                Just (Right _) -> do
                  liftIO $ atomically $ writeTVar changed True
                  incrementCount
                  runUpdate resolver
        
        threads <- liftIO $ mapM (\_ -> async $ withResolver dnsResolvSeed $ \resolver -> runStderrLoggingT $ runUpdate resolver) [1..dnsThreads (dns config)]
        liftIO $ mapM_ wait threads
        finalChanged <- liftIO $ atomically $ readTVar changed
        finalCount <- liftIO $ atomically $ readTVar count
        if finalChanged
          then do
            entries <- liftIO $ DNSCache.getEntries cache
            let !newSet = IPSet.fromIPList $ mconcat $ map (Set.toList . DNSCache.ips . snd) $ Map.toList entries
            $(logInfo) [qq|DNS refresh finished, total DNS entries: {Map.size entries}, total A entries: {IPSet.size newSet}, requests sent: {finalCount}|]
            liftIO $ atomically $ writeTEVar dnsSet newSet
          else do
            $(logInfo) [qq|DNS refresh finished, no new entries|]
      -- Periodically request DNS updates
      periodicallyUpdateDns = do
        let (MkFixed (fromInteger -> delay)) = fromRational $ toRational $ dnsInterval $ dns config :: Micro
        forever $ do
          liftIO $ threadDelay delay
          $(logInfo) [qq|Starting periodic DNS update|]
          liftIO $ DNSCache.queueExpired cache
          requestDnsUpdate

  let writeOutputs set = do
        $(logInfo) "Writing updated IP set"
        let set' = foldr IPSet.deleteRange set reservedRanges
        mask_ $ mapM_ (writeOutput set') $ outputs config

      updateSet oldResult = do
        (currDnsSet, currLists) <- liftIO $ atomically $ do
          (dnsSetUpdated, currDnsSet) <- readTEVar dnsSet
          currSources <- mapM readTEVar sources
          when (not dnsSetUpdated && not (any fst currSources)) retry
          return (currDnsSet, map snd currSources)

        let newDomains = mconcat $ map domainsSet currLists
            newResult = foldr (IPSet.unionSymmetric . ips) currDnsSet currLists

        liftIO $ DNSCache.setDomains cache newDomains
        when (newResult /= oldResult) $ writeOutputs newResult
        updateSet newResult

  _ <- forkIO $ when False $ runStderrLoggingT waitAndUpdateDns `catchAll` throwTo tid
  _ <- forkIO $ when False $ runStderrLoggingT periodicallyUpdateDns `catchAll` throwTo tid
  
  runStderrLoggingT $ do
    initialLists <- mapM (liftIO . atomically . waitTEVar) sources
    let initialDomains = mconcat $ map domainsSet initialLists
    let initialResult = foldr (IPSet.unionSymmetric . ips) mempty initialLists
    writeOutputs initialResult
    liftIO $ DNSCache.setDomains cache initialDomains
    requestDnsUpdate
    updateSet initialResult
