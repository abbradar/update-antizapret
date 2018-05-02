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
  <> (mconcat $ map convertIpsGroup ipGroups)
  <> "};\n\n"
  <> "var SUBNETS = ["
  <> (mconcat $ map convertSubnet subnets)
  <> "];\n\n"
  <> "var SPLIT_SHIFT = " <> BSBuilder.intDec splitShift <> ";\n"
  <> "var SPLIT_MASK = 0x" <> BSBuilder.word32Hex splitMask <> ";\n"

  where (ipsList, subnetsList) = partitionEithers $ map splitSubnets $ IPSet.toAscList iset
        subnets = sortBy (comparing mlen) subnetsList
        ipGroups = groupBy (\a b -> (a `shiftR` splitShift) == (b `shiftR` splitShift)) $ map (\(IP4 ipInt) -> ipInt) ipsList

        splitSubnets ipr@(addrRangePair -> (ip, mlen))
          | mlen == 32 = Left ip
          | otherwise = Right ipr
          
        convertIpsGroup ipsGroup@(ipInt : _) = BSBuilder.word32Dec (ipInt `shiftR` splitShift) <> ":["
                                               <> (mconcat $ map convertIp ipsGroup) <> "],"
        convertIpsGroup [] = error "convertIpsGroup: impossible"

        convertIp ipInt = BSBuilder.word32Dec (ipInt .&. splitMask) <> ","
        convertSubnet (AddrRange { addr = IP4 ipInt, mask = IP4 maskInt }) = "[0x" <> BSBuilder.word32Hex ipInt <> ",0x" <> BSBuilder.word32Hex maskInt <> "],"

        splitShift = 16
        splitMask = (1 `shiftL` splitShift) - 1
