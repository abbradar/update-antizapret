module Antizapret.Output.PAC
  ( toPACGlobals
  ) where

import Data.Monoid
import Data.List
import Data.Bits
import Data.Ord
import Data.Either
import qualified Data.ByteString.Builder as BSBuilder
import Data.IP
import Data.IP.Internal

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet

toPACGlobals :: IPv4Set -> BSBuilder.Builder
toPACGlobals iset =
     "var ADDRESSES = {"
  <> mapConcatWithCommas convertIpsGroup ipGroups
  <> "};\n\n"
  <> "var SUBNETS = {"
  <> mapConcatWithCommas convertSubnetsGroup subnetGroups
  <> "};\n\n"
  <> "var SPLIT_SHIFT = " <> BSBuilder.intDec splitShift <> ";\n"
  <> "var SPLIT_MASK = 0x" <> BSBuilder.word32Hex splitMask <> ";\n"

  where (ipsList, subnetsList) = partitionEithers $ map splitSubnets $ IPSet.toAscList iset
        subnetGroups = groupBy (\a b -> mlen a == mlen b) $ sortBy (comparing mlen) subnetsList
        ipGroups = groupBy (\a b -> (a `shiftR` splitShift) == (b `shiftR` splitShift)) $ map (\(IP4 ipInt) -> ipInt) ipsList

        splitSubnets ipr@(addrRangePair -> (ip, mlen))
          | mlen == 32 = Left ip
          | otherwise = Right ipr
          
        convertIpsGroup ipsGroup@(ipInt : _) = [BSBuilder.word32Dec (ipInt `shiftR` splitShift), ":[", mapConcatWithCommas convertIpRange ipsRanges, "]"]
          where ipsRanges = groupRanges ipsGroup
        convertIpsGroup [] = error "convertIpsGroup: impossible"

        convertIpRange (from, to)
          | from == to = [renderSplitIp from]
          | from + 1 == to = [renderSplitIp from, ",", renderSplitIp to]
          | otherwise = ["[", renderSplitIp from, ",", renderSplitIp to, "]"]

        convertSubnetsGroup subnetsGroup@(ipr : _) = [BSBuilder.intDec (mlen ipr), ":[", mapConcatWithCommas convertSubnet subnetsGroup, "]"]
        convertSubnetsGroup [] = error "convertIpsGroup: impossible"

        convertSubnet (AddrRange { addr = IP4 ipInt, mlen }) = [BSBuilder.word32Dec (ipInt `shiftR` (32 - mlen))]

        splitShift = 16
        splitMask = (1 `shiftL` splitShift) - 1

        mapConcatWithCommas f = mconcat . concat . intersperse [","] . map f

        renderSplitIp ip = BSBuilder.word32Dec (ip .&. splitMask)

        groupRanges [] = []
        groupRanges (from:others) = (from, to) : groupRanges left
          where (to, left) = go from others

                go current [] = (current, [])
                go current list@(next:t)
                  | current + 1 == next = go next t
                  | otherwise = (current, list)
