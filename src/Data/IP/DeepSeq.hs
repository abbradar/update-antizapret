{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IP.DeepSeq where

import Data.IP
import Control.DeepSeq

instance NFData IPv4
instance NFData a => NFData (AddrRange a)
