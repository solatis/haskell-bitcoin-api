module Network.Bitcoin.Api.WalletSpec where

import qualified Data.List                                    as L (find)
import           Data.Maybe                                   (isJust)
import qualified Data.Text                                    as T (pack)

import qualified Network.Bitcoin.Api.Wallet               as Wallet
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing wallet functions" $ do
   it "should be able list unspent transactions" $ do
     r <- testClient Wallet.listUnspent
     length r `shouldSatisfy` (>= 1)

   it "should be able list all accounts" $ do
     r <- testClient Wallet.listAccounts
     length r `shouldSatisfy` (>= 1)

   it "should be able to create a new address under the default account" $ do
     testClient $ \client -> do
       addr <- Wallet.newAddress client
       acc  <- Wallet.getAddressAccount client addr

       acc `shouldBe` (T.pack "")

   it "should be able to create a new address under a specific account" $ do
     testClient $ \client -> do
       addr <- Wallet.newAddressWith client (T.pack "testAccount")
       acc  <- Wallet.getAddressAccount client addr

       acc `shouldBe` (T.pack "testAccount")

       -- Extra validation that the account also appears in the wallet
       list <- Wallet.listAccounts client
       L.find (\(needle, _) -> needle == T.pack "testAccount") list `shouldSatisfy` isJust

   it "should be able to create a change address" $ do
     testClient $ \client -> do
       addr <- Wallet.newChangeAddress client
       acc <-  Wallet.getAddressAccount client addr

       acc `shouldBe` (T.pack "")
