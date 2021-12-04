import qualified Data.ByteString.Lazy as LB
import qualified Data.Attoparsec.ByteString as AP
import qualified Codec.Text.IConv as IConv
import Criterion.Main

import Antizapret.Format.ZapretInfo

main :: IO ()
main = do
  dump <- LB.toStrict <$> IConv.convert "cp1251" "utf-8" <$> LB.readFile "data/dump.short.csv"
  let runParser raw = runParser' (AP.parse zapretInfo) raw
      runParser' parser raw  =
        case parser raw of
          AP.Fail _ _ err -> error err
          AP.Partial parser' -> runParser' parser' ""
          AP.Done _ r -> r

  -- let rawList = runParser dump
  -- putStrLn $ "IPs and ranges count: " <> show (IPSet.size $ ips rawList)
  -- putStrLn $ "Domains count: " <> show (S.size $ domains rawList)
  -- putStrLn $ "Domain wildcards count: " <> show (S.size $ domainWildcards rawList)

  defaultMain [ bench "parse and normalize" $ nf runParser dump
              ]
