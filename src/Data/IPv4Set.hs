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
  , delete'
  , delete
  , deleteRange
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
import Data.Word
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

minimizeRanges :: Word32 -> Int -> IntMap (AddrRange IPv4) -> IntMap (AddrRange IPv4)
minimizeRanges _ 0 _ = IM.singleton 0 $ makeAddrRange (IP4 0) 0
minimizeRanges ipAddr len iset =
  case IM.lookup coIpKey iset of
    Just (addrRangePair -> (_, coLen))
      | coLen == len ->
          let smallerSet = IM.delete coIpKey iset
              newIp = ipAddr `clearBit` bitPos
          in minimizeRanges newIp (len - 1) smallerSet
    _ -> IM.insert ipKey (makeAddrRange (IP4 ipAddr) len) iset
  where bitPos = finiteBitSize ipAddr - len
        coIpAddr = ipAddr `complementBit` bitPos
        ipKey = fromIntegral ipAddr
        coIpKey = fromIntegral coIpAddr

-- Returns if the set has changed.
insert' :: IPv4 -> IPv4Set -> (Bool, IPv4Set)
insert' ip@(IP4 ipAddr@(fromIntegral -> ipInt)) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> (True, iset')
    Just (_, range)
      | not (ip `isMatchedTo` range) -> (True, iset')
      | otherwise -> (False, iset)
  where iset' = IPv4Set $ minimizeRanges ipAddr (finiteBitSize ipAddr) $ ipSet iset

insert :: IPv4 -> IPv4Set -> IPv4Set
insert ip iset = snd (insert' ip iset)

insertRange' :: AddrRange IPv4 -> IPv4Set -> (Bool, IPv4Set)
insertRange' ipr@(AddrRange { addr = IP4 ipAddr@(fromIntegral -> ipInt), mlen }) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> (True, iset'')
    Just (_, range)
      | not (range >:> ipr) -> (True, iset'')
      | otherwise -> (False, iset)
  where iset' = cleanupRange ipr $ ipSet iset
        iset'' = IPv4Set $ minimizeRanges ipAddr mlen iset'

insertRange :: AddrRange IPv4 -> IPv4Set -> IPv4Set
insertRange ipr iset = snd (insertRange' ipr iset)

cleanupRange :: AddrRange IPv4 -> IntMap (AddrRange IPv4) -> IntMap (AddrRange IPv4)
cleanupRange ipr@(AddrRange { addr = IP4 ipAddr@(fromIntegral -> ipInt), mlen }) iset
  | mlen == finiteBitSize ipAddr = iset
  | otherwise = clean iset
  where clean im = case IM.lookupGE ipInt im of
          Nothing -> im
          Just (k, range)
            | ipr >:> range -> clean $ IM.delete k im
            | otherwise -> im

expandCoRanges :: Word32 -> Int -> Int -> IntMap (AddrRange IPv4) -> IntMap (AddrRange IPv4)
expandCoRanges ipAddr len currLen iset
  | currLen == len = iset
  | otherwise = expandCoRanges ipAddr len (currLen + 1) iset'
  where bitPos = finiteBitSize ipAddr - currLen - 1
        coIpAddr = ipAddr `complementBit` bitPos
        coIpKey = fromIntegral coIpAddr
        coRange = makeAddrRange (IP4 coIpAddr) (currLen + 1)
        iset' = IM.insert coIpKey coRange iset

delete' :: IPv4 -> IPv4Set -> (Bool, IPv4Set)
delete' ip@(IP4 ipAddr@(fromIntegral -> ipInt)) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> (False, iset)
    Just (key, range)
      | not (ip `isMatchedTo` range) -> (False, iset)
      | otherwise ->
        let (_, len) = addrRangePair range
            iset' = IM.delete key $ ipSet iset
            iset'' = IPv4Set $ expandCoRanges ipAddr (finiteBitSize ipAddr) len iset'
        in (True, iset'')

delete :: IPv4 -> IPv4Set -> IPv4Set
delete ip iset = snd (delete' ip iset)

deleteRange :: AddrRange IPv4 -> IPv4Set -> IPv4Set
deleteRange ipr@(AddrRange { addr = IP4 ipAddr@(fromIntegral -> ipInt), mlen }) iset =
  case IM.lookupLE ipInt $ ipSet iset of
    Nothing -> cleanedIset
    Just (key, range@(AddrRange { mlen = rangeLen }))
      | not (range >:> ipr) -> cleanedIset
      | otherwise -> IPv4Set $ expandCoRanges ipAddr mlen rangeLen $ IM.delete key $ ipSet iset
  where cleanedIset = IPv4Set $ cleanupRange ipr $ ipSet iset

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
fromIPList = foldr insert empty

toIPList :: IPv4Set -> [IPv4]
toIPList = concatMap maskedAddresses . toList
