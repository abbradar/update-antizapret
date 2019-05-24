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
import qualified Data.ByteString.Short as Short
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Control.Applicative
import Control.Monad
import qualified Data.Set as Set
import Data.Attoparsec.Text
import Data.IP (IPv4, AddrRange)
import qualified Data.IP as IP
import qualified Text.IDNA as IDNA

import qualified Data.IPv4Set as IPSet
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

normalizeDomain :: Text -> Maybe Text
normalizeDomain full =
  case Text.unsnoc full of
    Nothing -> Nothing
    Just (trimmed, c) -> fmap (Text.intercalate ".") $ mapM convertOne $ Text.split (== '.') t
      where t = if c == '.' then trimmed else full

            convertOne "" = Nothing
            convertOne x = IDNA.toASCII False True x

domain :: Parser ShortDomain
-- Simple
domain = do
  str <- takeWhile1 domainChar
  case normalizeDomain str of
    Nothing -> fail "domain: invalid code points in domain"
    Just dom -> return $ Short.toShort $ Text.encodeUtf8 dom

domainRange :: Parser ShortDomainRange
domainRange = string "*." >> domain

-- Helpers
ipv4OrRangeSingle :: Parser RawBlockList
ipv4OrRangeSingle = do
  res <- ipv4OrRange
  return $ case res of
    Left ip -> mempty { ips = IPSet.singleton ip }
    Right ipr -> mempty { ips = IPSet.singletonRange ipr }

domainOrRangeSingle :: Parser RawBlockList
domainOrRangeSingle = do
  res <- Left <$> domainRange <|> Right <$> domain
  return $ case res of
    Left domRange -> mempty { domainWildcards = Set.singleton domRange }
    Right dom -> mempty { domains = Set.singleton dom }

entrySingle :: Parser a -> Parser RawBlockList
entrySingle end = (ipv4OrRangeSingle <* end) <|> (domainOrRangeSingle <* end)

-- Utilities
skipLine :: Parser ()
skipLine = skipWhile (not . isEndOfLine) <* optional endOfLine
