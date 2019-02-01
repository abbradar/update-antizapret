module Antizapret.Output.IPSet
  ( toIPSetList
  ) where

import qualified Data.ByteString.Builder as BSBuilder
import Data.IP

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet
import Antizapret.Render

toIPSetList :: IPv4Set -> BSBuilder.Builder
toIPSetList = mconcat . map convert . IPSet.toList
  where convert (addrRangePair -> (ip, mlen)) = line <> BSBuilder.char7 '\n'
          where ipRender = renderIPv4 ip
                line
                  | mlen == 32 = ipRender
                  | otherwise = ipRender <> BSBuilder.char7 '/' <> BSBuilder.intDec mlen
