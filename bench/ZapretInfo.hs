import Data.Foldable
import Data.Monoid
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Attoparsec.Text.Lazy as AP
import qualified Codec.Text.IConv as IConv
import Criterion.Main

import qualified Data.IPv4Set as IPSet
import Antizapret.Format.ZapretInfo
import Antizapret.Types

main :: IO ()
main = do
  dump <- LT.decodeUtf8 <$> IConv.convert "cp1251" "utf-8" <$> LB.readFile "data/dump.csv"
  let runParser raw =
        case AP.parse zapretInfo raw of
          AP.Fail _ _ err -> error err
          AP.Done _ r -> r

      rawList = runParser dump
      normalized = ipsSet rawList

  putStrLn $ "IPs count: " <> show (S.size $ ips rawList)
  putStrLn $ "IP ranges count: " <> show (S.size $ ipRanges rawList)
  putStrLn $ "Domains count: " <> show (S.size $ domains rawList)
  putStrLn $ "Domain wildcards count: " <> show (S.size $ domainWildcards rawList)
  putStrLn $ "Normalized count: " <> show (IPSet.size normalized)

  defaultMain [ bench "parse" $ nf runParser dump
              , bench "normalize" $ nf ipsSet rawList
              ]
