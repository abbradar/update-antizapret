module Antizapret.Filter.Coarse
  ( coarseIPSet
  ) where

import Data.IP

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet

-- Trim specificity of an IP address set to a specified bit length.
-- e.g. two IP addresses 192.168.1.14 and 192.168.1.72 with coarse bit length 24
-- would be combined into 192.168.1.0/24.
coarseIPSet :: Int -> IPv4Set -> IPv4Set
coarseIPSet specificity = IPSet.fromList . map coarseOne . IPSet.toList
  where coarseOne (addrRangePair -> (ip, len)) = makeAddrRange ip (min specificity len)
