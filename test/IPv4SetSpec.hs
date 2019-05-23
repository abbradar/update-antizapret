{-# OPTIONS_GHC -fno-warn-orphans #-}

module IPv4SetSpec (spec) where

import Data.Set (Set)
import qualified Data.Set as S
import Test.QuickCheck
import Test.Hspec

import Data.IP
import Data.IP.Internal

import qualified Data.IPv4Set as IS

instance Arbitrary IPv4 where
  arbitrary = IP4 <$> arbitrary

instance Arbitrary (AddrRange IPv4) where
  arbitrary = fst <$> arbitraryRangedIP

arbitraryRangedIP :: Gen (AddrRange IPv4, IPv4)
arbitraryRangedIP = do
  ip <- arbitrary
  range <- choose (0, 32)
  return (makeAddrRange ip range, ip)

arbitraryAdjastentIPs :: Gen (Set IPv4)
arbitraryAdjastentIPs = sized $ \len -> do
  IP4 ip <- arbitrary
  return $ S.fromList $ map (IP4 . (ip +)) [1 .. fromIntegral len]

spec :: Spec
spec = do
  describe "IPv4Set" $ do
    it "simplifies inserted ip already in range" $
      forAll arbitraryRangedIP $ \(range, ip) -> (IS.toList $ IS.insert ip $ IS.singletonRange range) == [range]
    it "simplifies ips when range is inserted" $
      forAll arbitraryRangedIP $ \(range, ip) -> (IS.toList $ IS.insertRange range $ IS.singleton ip) == [range]
    it "doesn't corrupt its contents" $
      forAll arbitraryAdjastentIPs $ \ips -> (S.fromList $ IS.toIPList $ IS.fromIPList $ S.toList ips) == ips
