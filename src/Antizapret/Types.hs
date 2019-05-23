module Antizapret.Types where

import Data.Semigroup
import Data.Foldable
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Data.Set (Set)
import Data.IP
import Control.DeepSeq
import Network.DNS.Types

import Data.IPv4Set (IPv4Set)
import qualified Data.IPv4Set as IPSet

-- All subdomains of a given domain.
type DomainRange = Domain

data RawBlockList = RawBlockList { ips :: !(Set IPv4)
                                 , ipRanges :: !(Set (AddrRange IPv4))
                                 , domains :: !(Set Domain)
                                 , domainWildcards :: !(Set DomainRange)
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

domainsSet :: RawBlockList -> Set Domain
domainsSet list = domains list <> domainWildcards list
