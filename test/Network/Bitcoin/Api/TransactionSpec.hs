module Network.Bitcoin.Api.TransactionSpec where

import           Data.Maybe                                   (mapMaybe)

import           Control.Lens                                 ((^.))

import qualified Data.Bitcoin.Transaction                     as Btc

import qualified Network.Bitcoin.Api.Dump                     as Dump
import qualified Network.Bitcoin.Api.Mining                   as Mining
import qualified Network.Bitcoin.Api.Transaction              as Transaction
import           Network.Bitcoin.Api.Types.UnspentTransaction (address, amount)
import qualified Network.Bitcoin.Api.Wallet                   as Wallet

import           Network.Bitcoin.Api.TestUtil                 (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing transaction functions" $ do
    it "can create transaction" $ do
      testClient $ \client -> do
        utxs <- Wallet.listUnspent client
        addr <- Wallet.newAddress client
        tx   <- Transaction.create client utxs [(addr, 50)]

        case tx of
         (Btc.Transaction 1 _ _ 0) -> return ()
         _ -> expectationFailure ("Result does not match expected: " ++ show tx)

    it "can sign transaction without providing any input transactions" $ do
      testClient $ \client -> do
        utxs           <- Wallet.listUnspent client
        addr           <- Wallet.newAddress client
        tx             <- Transaction.create client utxs [(addr, 50)]
        (_, completed) <- Transaction.sign client tx Nothing Nothing

        completed `shouldBe` True

    it "can sign transaction when providing any input transactions" $ do
      testClient $ \client -> do
        utxs           <- Wallet.listUnspent client
        addr           <- Wallet.newAddress client
        tx             <- Transaction.create client utxs [(addr, 50)]
        (_, completed) <- Transaction.sign client tx (Just utxs) Nothing

        completed `shouldBe` True

    it "can sign transaction when providing any explicit signing key" $ do
      testClient $ \client -> do
        utxs           <- Wallet.listUnspent client
        addr           <- Wallet.newAddress client

        -- Generates an array of private keys of all the input addresses we use
        keys           <- mapM (Dump.getPrivateKey client) $ mapMaybe (^. address) utxs

        tx             <- Transaction.create client utxs [(addr, 50)]
        (_, completed) <- Transaction.sign client tx Nothing (Just keys)

        -- This is an important check, since it validates that we are using the
        -- correct keys and our manual signing works properly.
        completed `shouldBe` True

    it "can send a transaction" $ do
      testClient $ \client -> do
        utxs             <- Wallet.listUnspent client

        (length utxs) `shouldSatisfy` (>= 1)

        -- Calculate the total BTC of all unspent transactions
        let btc          = foldr (+) 0 $ map (^. amount) utxs

        addr             <- Wallet.newAddress client
        tx               <- Transaction.create client utxs [(addr, (btc - 0.0001))]
        (tx', completed) <- Transaction.sign client tx (Just utxs) Nothing

        completed `shouldBe` True

        txid             <- Transaction.send client tx'
        putStrLn ("txid = " ++ show txid)
        True `shouldBe` True

    it "can list transactions" $ do
      -- Generate some blocks, so we know for sure that some transactions are in
      -- some blocks.
      _   <- testClient $ \client -> Mining.generate client 10
      txs <- testClient $ \client -> Transaction.list client Nothing Nothing

      -- :TODO: validate that there transactions are in chronological order
      length (txs) `shouldSatisfy` (>= 1)

    it "can watch for new transactions" $ do
      True `shouldBe` True
