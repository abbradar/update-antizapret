module Antizapret.Types where

import Data.Semigroup
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Data.Set (Set)
import Control.DeepSeq
import Network.DNS.Types

import Data.IPv4Set (IPv4Set)

-- All subdomains of a given domain.
type DomainRange = Domain

data RawBlockList = RawBlockList { ips :: !IPv4Set
                                 , domains :: !(Set Domain)
                                 , domainWildcards :: !(Set DomainRange)
                                 }
                  deriving (Show, Eq, Generic)

instance NFData RawBlockList

instance Semigroup RawBlockList where
  a <> b = RawBlockList { ips = ips a <> ips b
                        , domains = domains a <> domains b
                        , domainWildcards = domainWildcards a <> domainWildcards b
                        }

instance Monoid RawBlockList where
  mempty = RawBlockList { ips = mempty
                        , domains = mempty
                        , domainWildcards = mempty
                        }
  mappend = (Data.Semigroup.<>)

domainsSet :: RawBlockList -> Set Domain
domainsSet list = domains list <> domainWildcards list
