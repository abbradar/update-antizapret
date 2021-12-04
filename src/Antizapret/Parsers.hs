module Antizapret.Parsers
  ( normalizeDomain

  , ipv4
  , ipv4Range
  , ipv4OrRange

  , ipv6
  , ipv6Range
  , ipv6OrRange

  , domain
  , domainRange

  , ipOrRangeSingle
  , domainOrRangeSingle
  , entrySingle

  , domainChar

  , skipLine
  , manyFold'
  , sepByFold'
  , sepBy1Fold'
  ) where

import Data.Functor
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as Short
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Set as Set
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as AB
import Data.IP (Addr, IPv4, IPv6, AddrRange)
import qualified Data.IP as IP
import qualified Text.IDNA as IDNA

import qualified Data.IPv4Set as IPSet
import Antizapret.Types

normalizeDomain :: ByteString -> Maybe ByteString
normalizeDomain raw =
  case Text.unsnoc full of
    Nothing -> Nothing
    Just (trimmed, c) -> fmap (Text.encodeUtf8 . Text.intercalate ".") $ mapM convertOne $ Text.split (== '.') t
      where t = if c == '.' then trimmed else full

            convertOne "" = Nothing
            convertOne x = IDNA.toASCII False True x
  where full = Text.decodeUtf8 raw

octet :: Parser Int
octet = do
  n <- decimal
  when (n > 255) $ fail "octet: too big"
  return n

rangevN :: Int -> Parser Int
rangevN bits = do
  _ <- char8 '/'
  range <- decimal
  when (range > bits) $ fail "rangevN: range too big"
  return range

ipvNOrRange :: Addr a => Parser a -> Int -> Parser (Either a (AddrRange a))
ipvNOrRange parser bits = do
  ip <- parser
  mr <- optional (rangevN bits)
  return $ case mr of
    Just r -> Right $ IP.makeAddrRange ip r
    Nothing -> Left ip

ipv4 :: Parser IPv4
ipv4 = do
  a <- octet <* char8 '.'
  b <- octet <* char8 '.'
  c <- octet <* char8 '.'
  d <- octet
  return $ IP.toIPv4 [a, b, c, d]

rangev4 :: Parser Int
rangev4 = rangevN 32

ipv4Range :: Parser (AddrRange IPv4)
ipv4Range = IP.makeAddrRange <$> ipv4 <*> rangev4

ipv4OrRange :: Parser (Either IPv4 (AddrRange IPv4))
ipv4OrRange = ipvNOrRange ipv4 32

field16 :: Parser Int
field16 = do
  n <- hexadecimal
  when (n > 65535) $ fail "field16: too big"
  return n

ipv6 :: Parser IPv6
ipv6 = do
  firstParts <- field16 `sepBy` char8 ':'
  lastParts <- optional $ do
    _ <- string "::"
    field16 `sepBy` char8 ':'
  let addrLen = 8
      parts = firstParts ++ maybe [] (\l -> replicate (addrLen - length firstParts - length l) 0 ++ l) lastParts
  when (length parts /= addrLen) $ fail "ipv6: invalid number of fields"
  return $ IP.toIPv6 parts

rangev6 :: Parser Int
rangev6 = rangevN 128

ipv6Range :: Parser (AddrRange IPv6)
ipv6Range = IP.makeAddrRange <$> ipv6 <*> rangev6

ipv6OrRange :: Parser (Either IPv6 (AddrRange IPv6))
ipv6OrRange = ipvNOrRange ipv6 128

domainChar :: Char -> Bool
domainChar x = isAlpha_ascii x || isDigit x || x == '-' || x == '.' || x == '_' || x >= '\x80'

domain :: Parser ShortDomain
-- Simple
domain = do
  rawStr <- takeWhile1 domainChar
  case normalizeDomain rawStr of
    Nothing -> fail "domain: invalid code points in domain"
    Just dom -> return $ Short.toShort dom

domainRange :: Parser ShortDomainRange
domainRange = string "*." >> domain

-- Helpers
ipOrRangeSingle :: Parser RawBlockList
ipOrRangeSingle = v4 <|> v6
  where v4 = do
          res <- ipv4OrRange
          let !ret =
                case res of
                  Left ip -> mempty { ips = IPSet.singleton ip }
                  Right ipr -> mempty { ips = IPSet.singletonRange ipr }
          return ret
        -- FIXME
        v6 = ipv6OrRange $> mempty

domainOrRangeSingle :: Parser RawBlockList
domainOrRangeSingle = do
  res <- Left <$> domainRange <|> Right <$> domain
  let !ret =
        case res of
          Left domRange -> mempty { domainWildcards = Set.singleton domRange }
          Right dom -> mempty { domains = Set.singleton dom }
  return ret

entrySingle :: Parser a -> Parser RawBlockList
entrySingle end = (ipOrRangeSingle <* end) <|> (domainOrRangeSingle <* end)

-- Utilities

skipLine :: Parser ()
skipLine = AB.skipWhile (not . isEndOfLine) <* optional endOfLine

manyFold' :: (Monoid r, MonadPlus m) => m r -> m r
manyFold' p = go mempty
  where go !ret = next `mplus` return ret
          where next = do
                  curr <- p
                  go (curr <> ret)

sepByFold' :: (Monoid r, MonadPlus m) => m r -> m s -> m r
sepByFold' p s = sepBy1Fold' p s `mplus` return mempty

sepBy1Fold' :: (Semigroup r, MonadPlus m) => m r -> m s -> m r
sepBy1Fold' p s = p >>= go
  where go !ret = next `mplus` return ret
          where next = do
                  void s
                  curr <- p
                  go (ret <> curr)
