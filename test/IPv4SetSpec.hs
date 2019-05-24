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

arbitraryNestedRanges' :: Int -> Gen (AddrRange IPv4, AddrRange IPv4)
arbitraryNestedRanges' from = do
  ip <- arbitrary
  range <- choose (from, 32)
  subRange <- choose (range, 32)
  return (makeAddrRange ip range, makeAddrRange ip subRange)

arbitraryRangedIP' :: Int -> Gen (AddrRange IPv4, IPv4)
arbitraryRangedIP' from = do
  ip <- arbitrary
  range <- choose (from, 32)
  return (makeAddrRange ip range, ip)

arbitraryRangedIP :: Gen (AddrRange IPv4, IPv4)
arbitraryRangedIP = arbitraryRangedIP' 0

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
    it "simplifies ranges of ips" $
      forAll (arbitraryRangedIP' 24) $ \(range, _) -> (IS.toList $ IS.fromIPList $ IS.maskedAddresses range) == [range]
    it "correctly deletes from range of ips" $
      forAll (arbitraryRangedIP' 24) $ \(range, ip) ->
                                         let ips = S.delete ip $ S.fromList $ IS.maskedAddresses range
                                         in (S.fromList $ IS.toIPList $ IS.delete ip $ IS.singletonRange range) == ips
    it "correctly deletes ranges from ranges" $
      forAll (arbitraryNestedRanges' 24) $ \(range, subRange) ->
                                             let rangeIps = S.fromList $ IS.maskedAddresses range
                                                 subRangeIps = S.fromList $ IS.maskedAddresses subRange
                                                 ips = rangeIps `S.difference` subRangeIps
                                             in (S.fromList $ IS.toIPList $ IS.deleteRange subRange $ IS.singletonRange range) == ips
    it "doesn't corrupt its contents" $
      forAll arbitraryAdjastentIPs $ \ips -> (S.fromList $ IS.toIPList $ IS.fromIPList $ S.toList ips) == ips
