import System.Mem
import Control.Concurrent
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.ByteString as AP
import qualified Codec.Text.IConv as IConv
import Weigh

import qualified Data.IPv4Set as IPSet
import Antizapret.Format.ZapretInfo
import Antizapret.Types

main :: IO ()
main = do
  dump <- LB.toStrict <$> IConv.convert "cp1251" "utf-8" <$> LB.readFile "data/dump.short.csv"

  let runParser raw = runParser' (AP.parse zapretInfo) raw
      runParser' parser raw  =
        case parser raw of
          AP.Fail _ _ err -> error err
          AP.Partial parser' -> runParser' parser' ""
          AP.Done _ r -> r

      copyList = IPSet.fromList . IPSet.toList
  let rawList = runParser dump

  -- putStrLn $ "Total size: " <> show (B.length dump)
  -- performGC

  -- putStrLn $ "IPs and ranges count: " <> show (IPSet.size $ ips rawList)
  -- putStrLn $ "Domains count: " <> show (S.size $ domains rawList)
  -- putStrLn $ "Domain wildcards count: " <> show (S.size $ domainWildcards rawList)

  -- performGC
  -- putStrLn "Done"
  -- threadDelay 9999999

  mainWith $ do
    setColumns [Case, Allocated, GCs, Live]
    func "parse and normalize" runParser dump
    func "rebuild ipset" copyList (ips rawList)
