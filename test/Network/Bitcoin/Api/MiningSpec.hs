module Network.Bitcoin.Api.MiningSpec where

import qualified Network.Bitcoin.Api.Blockchain               as Blockchain
import qualified Network.Bitcoin.Api.Mining                   as Mining
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing mining functions" $ do
   it "can generate blocks" $ do
     countBefore <- testClient Blockchain.getBlockCount
     r <- testClient $ \client -> do
       Mining.generate client 1

     countAfter <- testClient Blockchain.getBlockCount

     length r `shouldBe` 1
     (countBefore + 1) `shouldBe` countAfter
