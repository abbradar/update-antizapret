module Antizapret.DNS
  ( CacheEntry(..)
  , DNSCache
  , new
  , queueExpired
  , setDomains
  , updateNext
  , getEntries
  ) where

import Data.Maybe
import Control.Exception
import Control.Monad
import Network.DNS.Types
import Network.DNS.Resolver
import qualified Network.DNS.LookupRaw as DNS
import Data.Time.Clock
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet

data CacheEntry = CacheEntry { ips :: !IPv4Set
                             , expire :: !UTCTime
                             }
                  deriving (Show, Eq)

type CacheEntries = Map Domain CacheEntry

data DNSCache = DNSCache { entries :: TVar CacheEntries
                         , pending :: TVar (Set Domain)
                         , failed :: TVar (Set Domain)
                         , inProgress :: TVar (Set Domain)
                         }

new :: IO DNSCache
new = do
  entries <- newTVarIO Map.empty
  pending <- newTVarIO Set.empty
  failed <- newTVarIO Set.empty
  inProgress <- newTVarIO Set.empty
  return $ DNSCache {..}

queueExpired :: DNSCache -> IO ()
queueExpired (DNSCache {..}) = do
  time <- getCurrentTime
  atomically $ do
    currEntries <- readTVar entries
    let (goodEntries, stalledEntries) = Map.partition (\entry -> expire entry > time) currEntries
    currPending <- readTVar pending
    let !newPending = Set.union currPending (Map.keysSet stalledEntries)
    writeTVar entries goodEntries
    writeTVar pending newPending

setDomains :: DNSCache -> Set Domain -> IO ()
setDomains (DNSCache {..}) domains = atomically $ do
  currEntries <- readTVar entries
  currPending <- readTVar pending
  currFailed <- readTVar failed
  currInProgress <- readTVar inProgress

  let filteredEntries = Map.restrictKeys currEntries domains
      filteredPending = currPending `Set.difference` domains
      !filteredFailed = currFailed `Set.difference` domains
      filteredInProgress = currInProgress `Set.difference` domains

  let addIf domain =
        if not (Map.member domain filteredEntries
              || Set.member domain filteredPending
              || Set.member domain filteredFailed
              || Set.member domain filteredInProgress)
        then Set.insert domain
        else id

  let newPending = foldr addIf filteredPending domains

  writeTVar entries filteredEntries
  writeTVar pending newPending
  writeTVar failed filteredFailed
  writeTVar inProgress filteredInProgress

startNext :: DNSCache -> IO (Maybe Domain)
startNext (DNSCache {..}) = atomically $ do
  currPending <- readTVar pending
  case Set.minView currPending of
    Nothing -> return Nothing
    Just (curr, next) -> do
      writeTVar pending next
      modifyTVar' inProgress $ Set.insert curr
      return $ Just curr

returnInProgress :: DNSCache -> STM () -> Domain -> IO ()
returnInProgress (DNSCache {..}) op domain = atomically $ do
  currInProgress <- readTVar inProgress
  when (Set.member domain currInProgress) $ do
    let !newProgress = Set.delete domain currInProgress
    writeTVar inProgress newProgress
    op

moveBack :: DNSCache -> TVar (Set Domain) -> Domain -> IO ()
moveBack cache back domain = returnInProgress cache (modifyTVar' back $ Set.insert domain) domain

emptyExpireTime :: NominalDiffTime
emptyExpireTime = 60 * 60

updateNext :: DNSCache -> Resolver -> IO (Maybe (Either (Domain, DNSError) Domain))
updateNext cache@(DNSCache {..}) resolver = bracketOnError (startNext cache) (mapM_ (moveBack cache pending)) $ mapM $ \domain -> do
  time <- getCurrentTime

  let emptyEntry = CacheEntry { ips = IPSet.empty
                              , expire = addUTCTime emptyExpireTime time
                              }

  ret <- DNS.lookupRaw resolver domain A
  case ret of
    Left e@NameError -> do
      moveBack cache failed domain
      return $ Left (domain, e)
    Left e@IllegalDomain -> do
      moveBack cache failed domain
      return $ Left (domain, e)
    Left e -> do
      -- Treat this as an empty response
      returnInProgress cache (modifyTVar' entries $ Map.insert domain emptyEntry) domain
      return $ Left (domain, e)
    Right msg -> do
      let extractRecord (ResourceRecord { rrttl = ttl, rrtype = A, rdata = RD_A ip }) = Just (ttl, ip)
          extractRecord _ = Nothing

      let records = mapMaybe extractRecord $ answer msg
      let entry =
            if null records then emptyEntry
            else
              let expireSecs = foldr1 min $ map fst records
              in CacheEntry { ips = IPSet.fromIPList $ map snd records
                            , expire = addUTCTime (fromInteger $ toInteger expireSecs) time
                            }
      returnInProgress cache (modifyTVar' entries $ Map.insert domain entry) domain
      return $ Right domain

getEntries :: DNSCache -> IO CacheEntries
getEntries (DNSCache {..}) = atomically $ readTVar entries
