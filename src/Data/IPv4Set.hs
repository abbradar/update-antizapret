module Data.IPv4Set
  ( IPv4Set
  , RangeableAddr(..)
  , empty
  , size
  , singleton
  , singletonRange
  , insert'
  , insert
  , insertRange'
  , insertRange
  , member
  , memberRange
  , unionSymmetric
  , toList
  , fromList
  , toAscList
  , toDescList
  , fromIPList
  , toIPList
  ) where

import GHC.Generics (Generic)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Semigroup
import Data.Bits
import Data.IP
import Data.IP.Internal
import Control.DeepSeq

import Data.IP.DeepSeq ()

class Addr a => RangeableAddr a where
  maskedAddresses :: AddrRange a -> [a]

instance RangeableAddr IPv4 where
  maskedAddresses (addrRangePair -> (IP4 ipAddr, len)) = map (IP4 . (ipAddr +)) [0 .. count - 1]
    where count = 1 `shiftL` (finiteBitSize ipAddr - len)

newtype IPv4Set = IPv4Set { ipSet :: IntMap (AddrRange IPv4) }
             deriving (Show, Eq, Ord, Generic)

instance NFData IPv4Set

empty :: IPv4Set
empty = IPv4Set IM.empty

size :: IPv4Set -> Int
size = IM.size . ipSet

singleton :: IPv4 -> IPv4Set
singleton ip@(IP4 ipAddr@(fromIntegral -> ipInt)) = IPv4Set $ IM.singleton ipInt (makeAddrRange ip (finiteBitSize ipAddr))

singletonRange :: AddrRange IPv4 -> IPv4Set
singletonRange ipr@(AddrRange { addr = IP4 (fromIntegral -> ipInt) }) = IPv4Set $ IM.singleton ipInt ipr

-- Returns if the set has changed.
insert' :: IPv4 -> IPv4Set -> (Bool, IPv4Set)
insert' ip@(IP4 ipAddr@(fromIntegral -> ipInt)) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> (True, iset')
    Just (_, range)
      | not (ip `isMatchedTo` range) -> (True, iset')
      | otherwise -> (False, iset)
  where iset' = IPv4Set $ IM.insert ipInt (makeAddrRange ip (finiteBitSize ipAddr)) $ ipSet iset

insert :: IPv4 -> IPv4Set -> IPv4Set
insert ip iset = snd (insert' ip iset)

insertRange' :: AddrRange IPv4 -> IPv4Set -> (Bool, IPv4Set)
insertRange' ipr@(AddrRange { addr = IP4 ipAddr@(fromIntegral -> ipInt), mlen }) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> (True, iset')
    Just (_, range)
      | not (range >:> ipr) -> (True, iset')
      | otherwise -> (False, iset)
  where iset' = IPv4Set $ IM.insert ipInt ipr $ (if mlen == finiteBitSize ipAddr then id else clean) $ ipSet iset

        clean im = case IM.lookupGT ipInt im of
          Nothing -> im
          Just (k, range)
            | ipr >:> range -> clean $ IM.delete k im
            | otherwise -> im

insertRange :: AddrRange IPv4 -> IPv4Set -> IPv4Set
insertRange ipr iset = snd (insertRange' ipr iset)

member :: IPv4 -> IPv4Set -> Bool
member ip@(IP4 (fromIntegral -> ipInt)) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> False
    Just (_, range) -> ip `isMatchedTo` range

memberRange :: AddrRange IPv4 -> IPv4Set -> Bool
memberRange ipr@(AddrRange { addr = IP4 (fromIntegral -> ipInt) }) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> False
    Just (_, range) -> range >:> ipr

instance Semigroup IPv4Set where
  -- Performance greatly depends on the right operand being bigger.
  a <> b = foldr insertRange b $ map snd $ IM.toList $ ipSet a

instance Monoid IPv4Set where
  mempty = IPv4Set mempty
  mappend = (Data.Semigroup.<>)

-- Use when it's unknown whether left or right operand is bigger.
unionSymmetric :: IPv4Set -> IPv4Set -> IPv4Set
a `unionSymmetric` b = if size a > size b then b <> a else a <> b

toList :: IPv4Set -> [AddrRange IPv4]
toList iset = map snd $ IM.toList $ ipSet iset

toAscList :: IPv4Set -> [AddrRange IPv4]
toAscList iset = map snd $ IM.toAscList $ ipSet iset

toDescList :: IPv4Set -> [AddrRange IPv4]
toDescList iset = map snd $ IM.toDescList $ ipSet iset

fromList :: [AddrRange IPv4] -> IPv4Set
fromList = foldr insertRange empty

fromIPList :: [IPv4] -> IPv4Set
fromIPList = IPv4Set . IM.fromList . map (\ip@(IP4 ipAddr@(fromIntegral -> ipInt)) -> (ipInt, makeAddrRange ip (finiteBitSize ipAddr)))

toIPList :: IPv4Set -> [IPv4]
toIPList = concatMap maskedAddresses . toList
