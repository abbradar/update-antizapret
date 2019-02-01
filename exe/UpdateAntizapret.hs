import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import System.IO
import System.Environment
import Control.Concurrent (threadDelay, forkIO, throwTo, myThreadId)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSBuilder
import Control.Concurrent.STM
import Data.Text (Text)
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as JSON
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Control.Monad.Logger
import Control.Monad.IO.Class
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

import Paths_update_antizapret
import Antizapret.Types
import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet
import qualified Antizapret.Format.Simple as Format
import qualified Antizapret.Format.ZapretInfo as Format
import qualified Antizapret.Output.IPSet as Output
import qualified Antizapret.Output.PAC as Output

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
                                 , outputFormat :: OutputFormatConfig
                                 }
                  deriving (Show, Eq, Generic)

instance FromJSON OutputConfig where
  parseJSON = JSON.genericParseJSON $ jsonOptions "output"

data Config = Config { inputs :: [InputConfig]
                     , outputs :: [OutputConfig]
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
  $(logInfo) [qq|Watching for changes in directory {directory}, file {takeBaseName path}|]
  _ <- liftBaseOpDiscard (FSNotify.watchDir manager directory checkEvent) $ \_ -> run
  return ()

  where checkEvent (FSNotify.Removed _ _ _) = False
        checkEvent e | takeBaseName (FSNotify.eventPath e) == basename = True
        checkEvent _ = False

        directory = takeDirectory path
        basename = takeBaseName path

        run = runResourceT $ do
          sourceFile path `connect` sink

runInput :: forall m. MonadAZ m => InputConfig -> TMVar RawBlockList -> m ()
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
          result <- maybe mempty snd <$> await
          $(logInfo) [qq|Source {inputSource} updated|]
          liftIO $ atomically $ putTMVar resultVar result
        
        inputSink :: AZSink
        inputSink = formatConduit .| putResult
  
        expect :: AZSink -> m ()
        expect = case inputSource of
          SourceFeed {..} -> expectFeed sourceUrl sourceInterval sourceDataUrl
          SourceFilesystem {..} -> expectFilesystem sourcePath

writeOutput :: MonadAZ m => IPv4Set -> OutputConfig -> m ()
writeOutput set (OutputConfig {..}) = do
  rendered <- case outputFormat of
    IPSet -> return $ Output.toIPSetList set
    PAC {..} -> do
      templatePath <- liftIO $ getDataFileName "data/pac.template.js"
      template <- liftIO $ LBS.readFile templatePath
      return $ "var PROXY = " <> BSBuilder.lazyByteString (JSON.encode pacProxy) <> ";\n\n"
             <> Output.toPACGlobals set
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

  tid <- myThreadId
  sources <- forM (inputs config) $ \input -> do
    set <- newEmptyTMVarIO
    _ <- forkIO $ runStderrLoggingT (runInput input set) `catchAll` throwTo tid
    return set

  let writeOutputs set = do
        $(logInfo) "Writing updated IP set"
        mask_ $ mapM_ (writeOutput set) $ outputs config

      updateSet oldSets oldResult = do
        updatedSets <- liftIO $ atomically $ do
          raws <- mapM (tryTakeTMVar >=> (return . fmap ipsSet)) sources
          when (all isNothing raws) retry
          return raws

        let newSets = zipWith fromMaybe oldSets updatedSets
            newResult = foldr IPSet.unionSymmetric mempty newSets

        when (newResult /= oldResult) $ writeOutputs newResult
        updateSet newSets newResult

  runStderrLoggingT $ do
    initialSets <- map ipsSet <$> mapM (liftIO . atomically . takeTMVar) sources
    let initialResult = foldr IPSet.unionSymmetric mempty initialSets
    writeOutputs initialResult
    updateSet initialSets initialResult
