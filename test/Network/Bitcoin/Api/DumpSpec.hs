module Network.Bitcoin.Api.DumpSpec where

import qualified Network.Bitcoin.Api.Dump                     as Dump
import qualified Network.Bitcoin.Api.Wallet                   as Wallet

import           Network.Bitcoin.Api.TestUtil (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing import/dump functions" $ do
   it "should be able to dump private key" $ do
     testClient $ \client -> do
       addr <- Wallet.newAddress client
       r <- Dump.getPrivateKey client addr

       putStrLn ("r = " ++ show r)
       True `shouldBe` True
