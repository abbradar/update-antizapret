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
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import qualified Data.IPv4Set as IS
import Data.Attoparsec.Text
import Data.IP (IPv4, AddrRange)
import qualified Data.IP as IP

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
domainChar x = isAlpha x || isDigit x || inClass "-._" x

domain :: Parser TextDomain
-- Simple
domain = takeWhile1 domainChar

domainRange :: Parser TextDomainRange
domainRange = string "*." >> domain

-- Helpers
ipv4OrRangeSingle :: Parser BlockList
ipv4OrRangeSingle = do
  res <- ipv4OrRange
  return $ case res of
    Left ip -> mempty { ips = IS.singleton ip }
    Right ipr -> mempty { ips = IS.singletonRange ipr }

domainOrRangeSingle :: Parser BlockList
domainOrRangeSingle = do
  res <- Left <$> domainRange <|> Right <$> domain
  return $ case res of
    Left domRange -> mempty { domainWildcards = S.singleton domRange }
    Right dom -> mempty { domains = S.singleton dom }

entrySingle :: Parser a -> Parser BlockList
entrySingle end = (ipv4OrRangeSingle <* end) <|> (domainOrRangeSingle <* end)

-- Utilities
skipLine :: Parser ()
skipLine = skipWhile (not . isEndOfLine) <* optional endOfLine
