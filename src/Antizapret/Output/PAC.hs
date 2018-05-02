module Antizapret.Output.PAC
  ( toPACGlobals
  ) where

import Data.Monoid
import Data.List
import Data.Ord
import Data.Either
import qualified Data.ByteString.Builder as BSBuilder
import Data.IP
import Data.IP.Internal

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet
import Antizapret.Render

toPACGlobals :: IPv4Set -> BSBuilder.Builder
toPACGlobals iset =
     "var ADDRESSES = ["
  <> (mconcat $ map convertIp ips)
  <> "];\n\n"
  <> "var SUBNETS = ["
  <> (mconcat $ map convertSubnet subnets)
  <> "];\n"

  where (ips, subnets') = partitionEithers $ map splitSubnets $ IPSet.toList iset
        subnets = sortBy (comparing mlen) subnets'

        splitSubnets ipr@(addrRangePair -> (ip, mlen))
          | mlen == 32 = Left ip
          | otherwise = Right ipr
          
        convertIp (IP4 ipInt) = "0x" <> BSBuilder.word32Hex ipInt <> ","
        convertSubnet ipr = "[\"" <> renderIPv4 (addr ipr) <> "\",\"" <> renderIPv4 (mask ipr) <> "\"],"
