module Antizapret.Types where

import Data.Semigroup
import Data.Foldable
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Set (Set)
import Data.IP
import Control.DeepSeq

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet

type TextDomain = Text
-- All subdomains of a given domain.
type TextDomainRange = Text

data RawBlockList = RawBlockList { ips :: !(Set IPv4)
                                 , ipRanges :: !(Set (AddrRange IPv4))
                                 , domains :: !(Set TextDomain)
                                 , domainWildcards :: !(Set TextDomainRange)
                                 }
                  deriving (Show, Eq, Generic)

instance NFData RawBlockList

instance Semigroup RawBlockList where
  a <> b = RawBlockList { ips = ips a <> ips b
                        , ipRanges = ipRanges a <> ipRanges b
                        , domains = domains a <> domains b
                        , domainWildcards = domainWildcards a <> domainWildcards b
                        }

instance Monoid RawBlockList where
  mempty = RawBlockList { ips = mempty
                        , ipRanges = mempty
                        , domains = mempty
                        , domainWildcards = mempty
                        }
  mappend = (Data.Semigroup.<>)

ipsSet :: RawBlockList -> IPv4Set
ipsSet list = foldr IPSet.insertRange mempty (ipRanges list) <> IPSet.fromIPList (toList $ ips list)
