module Antizapret.Render
  ( renderIPv4
  ) where

import Data.Monoid
import Data.IP
import qualified Data.ByteString.Builder as BSBuilder

renderIPv4 :: IPv4 -> BSBuilder.Builder
renderIPv4 ip =    BSBuilder.intDec a <> "."
                <> BSBuilder.intDec b <> "."
                <> BSBuilder.intDec c <> "."
                <> BSBuilder.intDec d
  where [a, b, c, d] = fromIPv4 ip
