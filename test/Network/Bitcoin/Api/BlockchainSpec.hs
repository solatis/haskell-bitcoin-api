module Network.Bitcoin.Api.BlockchainSpec where

import qualified Network.Bitcoin.Api.Blockchain               as Blockchain
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing blockchain functions" $ do
   it "can request blockcount" $ do
     r <- testClient Blockchain.getBlockCount

     r `shouldSatisfy` (>= 100)

   it "can request block hashes" $ do
     testClient $ \client -> do
       count  <- Blockchain.getBlockCount client
       hashes <- mapM (Blockchain.getBlockHash client) [0..count - 1]

       fromIntegral (length (hashes)) `shouldBe` count

   it "can request blocks" $ do
     testClient $ \client -> do
       count  <- Blockchain.getBlockCount client
       blocks <- mapM (Blockchain.getBlock client) =<< mapM (Blockchain.getBlockHash client) [0..count - 1]

       fromIntegral (length (blocks)) `shouldBe` count
