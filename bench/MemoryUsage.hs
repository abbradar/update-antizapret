import System.Mem
import Control.Concurrent
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Attoparsec.Text.Lazy as AP
import qualified Codec.Text.IConv as IConv
import Weigh

import qualified Data.IPv4Set as IPSet
import Antizapret.Format.ZapretInfo
import Antizapret.Types

test :: IO ()
test = do
  -- dump <- LT.decodeUtf8 <$> IConv.convert "cp1251" "utf-8" <$> LB.readFile "data/dump.empty.csv"
  dump <- LT.decodeUtf8 <$> IConv.convert "cp1251" "utf-8" <$> LB.readFile "data/dump.csv"
  let runParser raw =
        case AP.parse zapretInfo raw of
          AP.Fail _ _ err -> error err
          AP.Done _ r -> r

  let rawList = runParser dump
  putStrLn $ "IPs and ranges count: " <> show (IPSet.size $ ips rawList)
  putStrLn $ "Domains count: " <> show (S.size $ domains rawList)
  putStrLn $ "Domain wildcards count: " <> show (S.size $ domainWildcards rawList)

main :: IO ()
main = do
  {-dump <- LT.decodeUtf8 <$> IConv.convert "cp1251" "utf-8" <$> LB.readFile "data/dump.csv"
  let runParser raw =
        case AP.parse zapretInfo raw of
          AP.Fail _ _ err -> error err
          AP.Done _ r -> r

      copyList = IPSet.fromList . IPSet.toList-}

  test
  performGC
  putStrLn "Done"
  threadDelay 99999999

  mainWith $ do
    setColumns [Case, Allocated, GCs, Live]
    --func "parse and normalize" runParser dump
    --func "rebuild ipset" copyList (ips rawList)
