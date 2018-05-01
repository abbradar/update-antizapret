module Antizapret.Render
  ( renderIPv4
  ) where

import Data.Monoid
import Data.IP
import qualified Data.ByteString.Builder as BSBuilder

renderIPv4 :: IPv4 -> BSBuilder.Builder
renderIPv4 ip =    BSBuilder.intDec a <> BSBuilder.char7 '.'
                <> BSBuilder.intDec b <> BSBuilder.char7 '.'
                <> BSBuilder.intDec c <> BSBuilder.char7 '.'
                <> BSBuilder.intDec d
  where [a, b, c, d] = fromIPv4 ip
