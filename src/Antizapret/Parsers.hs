module Antizapret.Parsers
  ( ipv4
  , ipv4Range
  , ipv4OrRange
  , domain
  , domainRange

  , ipv4OrRangeSingle
  , domainOrRangeSingle
  , entrySingle

  , domainChar

  , skipLine
  ) where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import Data.Attoparsec.Text
import Data.IP (IPv4, AddrRange)
import qualified Data.IP as IP
import qualified Text.IDNA as IDNA
import Network.DNS.Types

import Antizapret.Types

octet :: Parser Int
octet = do
  n <- decimal
  when (n > 255) $ fail "octet: too big"
  return n

rangev4 :: Parser Int
rangev4 = do
  _ <- char '/'
  range <- decimal
  when (range > 32) $ fail "ipv4Range: range too big"
  return range

ipv4 :: Parser IPv4
ipv4 = do
  a <- octet <* char '.'
  b <- octet <* char '.'
  c <- octet <* char '.'
  d <- octet
  return $ IP.toIPv4 [a, b, c, d]

ipv4Range :: Parser (AddrRange IPv4)
ipv4Range = IP.makeAddrRange <$> ipv4 <*> rangev4

ipv4OrRange :: Parser (Either IPv4 (AddrRange IPv4))
ipv4OrRange = do
  ip <- ipv4
  mr <- optional rangev4
  return $ case mr of
    Just r -> Right $ IP.makeAddrRange ip r
    Nothing -> Left ip

domainChar :: Char -> Bool
domainChar x = isAlpha x || isDigit x || x == '-' || x == '.' || x == '_'

normalizeDomain :: T.Text -> Maybe T.Text
normalizeDomain full =
  case T.unsnoc full of
    Nothing -> Nothing
    Just (trimmed, c) -> fmap (T.intercalate ".") $ mapM convertOne $ T.split (== '.') t
      where t = if c == '.' then trimmed else full
            convertOne x
              | T.null x = Nothing
              | otherwise = IDNA.toASCII False True x

domain :: Parser Domain
-- Simple
domain = do
  str <- takeWhile1 domainChar
  case normalizeDomain str of
    Nothing -> fail "domain: invalid code points in domain"
    Just dom -> return $ T.encodeUtf8 dom

domainRange :: Parser DomainRange
domainRange = string "*." >> domain

-- Helpers
ipv4OrRangeSingle :: Parser RawBlockList
ipv4OrRangeSingle = do
  res <- ipv4OrRange
  return $ case res of
    Left ip -> mempty { ips = S.singleton ip }
    Right ipr -> mempty { ipRanges = S.singleton ipr }

domainOrRangeSingle :: Parser RawBlockList
domainOrRangeSingle = do
  res <- Left <$> domainRange <|> Right <$> domain
  return $ case res of
    Left domRange -> mempty { domainWildcards = S.singleton domRange }
    Right dom -> mempty { domains = S.singleton dom }

entrySingle :: Parser a -> Parser RawBlockList
entrySingle end = (ipv4OrRangeSingle <* end) <|> (domainOrRangeSingle <* end)

-- Utilities
skipLine :: Parser ()
skipLine = skipWhile (not . isEndOfLine) <* optional endOfLine
