{-# LANGUAGE LambdaCase #-}

module Network.Bitcoin.ClientSpec where

import qualified Data.Bitcoin.Script                          as Btc
import qualified Data.Bitcoin.Transaction                     as Btc
import qualified Data.List                                    as L (find)
import           Data.Maybe                                   (isJust, mapMaybe)

import qualified Data.Text                                    as T (pack)

import           Network.HTTP.Client                          (HttpException (..))

import           Control.Lens                                 ((^.))
import           Network.Bitcoin.Rpc.Client

import qualified Network.Bitcoin.Rpc.Blockchain               as Blockchain
import qualified Network.Bitcoin.Rpc.Dump                     as Dump
import qualified Network.Bitcoin.Rpc.Mining                   as Mining
import qualified Network.Bitcoin.Rpc.Misc                     as Misc
import qualified Network.Bitcoin.Rpc.Transaction              as Transaction
import           Network.Bitcoin.Rpc.Types.UnspentTransaction (address, amount)
import qualified Network.Bitcoin.Rpc.Wallet                   as Wallet
import           Network.Wreq.Lens                            (statusCode)

import           Test.Hspec

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 18332 (T.pack "user") (T.pack "pass")

isStatusCodeException :: Int -> HttpException -> Bool
isStatusCodeException code (StatusCodeException s _ _) = s ^. statusCode == code
isStatusCodeException _ _ = False

spec :: Spec
spec = do
  describe "when creating a new client session" $ do
    it "callback returns generated value" $ do
      testClient (\_ -> return "foo") `shouldReturn` "foo"

    it "fails when providing invalid authentication credentials" $ do
      withClient "127.0.0.1" 18332 (T.pack "invaliduser") (T.pack "invalidpass") Misc.getInfo `shouldThrow` isStatusCodeException 401

  describe "when testing miscelaneous functions" $ do
   it "should be able to return server info" $ do
     r <- testClient Misc.getInfo

     r ^. Misc.bitcoinVersion `shouldBe` 100100
     r ^. Misc.bitcoindErrors `shouldBe` (T.pack "")

  describe "when testing mining functions" $ do
   it "can generate blocks" $ do
     countBefore <- testClient Blockchain.getBlockCount
     r <- testClient $ \client -> do
       Mining.generate client 1

     countAfter <- testClient Blockchain.getBlockCount

     length r `shouldBe` 1
     (countBefore + 1) `shouldBe` countAfter

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

  describe "when testing transaction functions" $ do
   it "can create transaction" $ do
     testClient $ \client -> do
       utxs <- Wallet.listUnspent client
       addr <- Wallet.newAddress client
       tx   <- Transaction.create client utxs [(addr, 50)]

       case tx of
        (Btc.Transaction 1 _ [(Btc.TransactionOut 5000000000 (Btc.Script _))] 0) -> return ()
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
     txs <- testClient $ \client -> Transaction.list client Nothing

     -- :TODO: validate that there transactions are in chronological order
     length (txs) `shouldSatisfy` (>= 1)

  describe "when testing import/dump functions" $ do
   it "should be able to dump private key" $ do
     testClient $ \client -> do
       addr <- Wallet.newAddress client
       r <- Dump.getPrivateKey client addr

       putStrLn ("r = " ++ show r)
       True `shouldBe` True
