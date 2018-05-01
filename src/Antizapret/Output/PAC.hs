module Antizapret.Output.PAC
  ( toPACGlobals
  ) where

import Data.Monoid
import Data.Either
import qualified Data.ByteString.Builder as BSBuilder
import Data.IP

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet
import Antizapret.Render

toPACGlobals :: IPv4Set -> BSBuilder.Builder
toPACGlobals iset =
     "var ADDRESSES = {\n"
  <> (mconcat $ map convertIp ips)
  <> "};\n\n"
  <> "var SUBNETS = [\n"
  <> (mconcat $ map convertSubnet subnets)
  <> "];\n"

  where (ips, subnets) = partitionEithers $ map splitSubnets $ IPSet.toList iset

        splitSubnets ipr@(addrRangePair -> (ip, mlen))
          | mlen == 32 = Left ip
          | otherwise = Right ipr
          
        convertIp ip = "  \"" <> renderIPv4 ip <> "\":null,\n"
        convertSubnet ipr = "  [\"" <> renderIPv4 (addr ipr) <> "\",\"" <> renderIPv4 (mask ipr) <> "\"],\n"
