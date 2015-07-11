{-# LANGUAGE OverloadedStrings #-}

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
       addr <- Wallet.newAddressWith client (T.pack "emptyTestAccount")
       acc  <- Wallet.getAddressAccount client addr

       acc `shouldBe` (T.pack "emptyTestAccount")

       -- Extra validation that the account also appears in the wallet
       list <- Wallet.listAccounts client
       L.find (\(needle, _) -> needle == T.pack "emptyTestAccount") list `shouldSatisfy` isJust

       -- Extra validation that we can look up the balance of the account
       balance <- Wallet.getAccountBalance client (T.pack "emptyTestAccount")
       balance `shouldBe` 0

   it "should be able to create a change address" $ do
     testClient $ \client -> do
       addr <- Wallet.newChangeAddress client

       acc <-  Wallet.getAddressAccount client addr
       acc `shouldBe` (T.pack "")

   it "should be able to move from one account to another" $ do
     testClient $ \client -> do
       _ <- Wallet.newAddressWith client (T.pack "testAccount")
       balance <- Wallet.getAccountBalance client ("")

       _ <- Wallet.move client "" "testAccount" 1 `shouldReturn` True

       -- Validate that, after the move, our main account's balance has been
       -- reduced with exactly 1 BTC (which is the amount we're moving).
       Wallet.getAccountBalance client ("") `shouldReturn` (balance - 1)
