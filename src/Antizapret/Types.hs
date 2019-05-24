module Antizapret.Types where

import Data.Semigroup
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Data.Set (Set)
import Data.ByteString.Short (ShortByteString)
import Control.DeepSeq
import Network.DNS.Types

import Data.IPv4Set (IPv4Set)

-- All subdomains of a given domain.
type DomainRange = Domain

type ShortDomain = ShortByteString
type ShortDomainRange = ShortDomain

data RawBlockList = RawBlockList { ips :: !IPv4Set
                                 , domains :: !(Set ShortDomain)
                                 , domainWildcards :: !(Set ShortDomainRange)
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

domainsSet :: RawBlockList -> Set ShortDomain
domainsSet list = domains list <> domainWildcards list
