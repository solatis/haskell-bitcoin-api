{-# LANGUAGE OverloadedStrings #-}

-- | This module provides functionality to manipulate raw transaction. It
--   automatically interprets transactions using the `bitcoin-tx` package, so
--   you can work with actual 'Btc.Transaction' objects rather than their
--   serialized format.

module Network.Bitcoin.Rpc.Transaction where

import           Data.Aeson
import           Data.Aeson.Lens

import           Control.Lens                                 ((^.), (^?))

import qualified Data.Bitcoin.Transaction                     as Btc

import qualified Network.Bitcoin.Internal                     as I
import qualified Network.Bitcoin.Rpc.Types                    as RT
import qualified Network.Bitcoin.Types                        as T

import           Network.Bitcoin.Rpc.Types.UnspentTransaction

-- | Creates a new transaction, but does not sign or submit it yet. You provide
--   a set of unspent transactions that you have the authority to spend, and you
--   provide a destination for all your bitcoins.
--
--   __WARNING: Check your math!__ If the sum of the Btc in unspent transactions
--   of your request is more than the sum of the Btc in the destinations, this
--   will be the miner's fee. It is reasonable to leave a small amount for the
--   miners, but if there is a large discrepancy between input and output, there
--   are no guarantees you will be warned.
--
--   All this function does is create a default script on how to spend coins from
--   one or more inputs to one or more outputs. Checking and verifying the
--   transaction will only happen when you actually submit the transaction to
--   the network.

create :: T.Client               -- ^ The client session we are using
       -> [UnspentTransaction]   -- ^ The inputs we are using for this transaction
       -> [(RT.Address, RT.Btc)] -- ^ A key/value pair which associates a
                                 --   destination address with a specific amount
                                 --   of bitcoins to send.
       -> IO Btc.Transaction
create client utxs outputs =
  let configuration = [toJSON (map txToOutpoint utxs), object (map outToAddress outputs)]

      txToOutpoint tx = object [
        ("txid", toJSON (tx ^. transactionId)),
        ("vout", toJSON (tx ^. vout))]

      outToAddress (addr, btc) = (addr, toJSON btc)

  in (return . Btc.decode) =<< I.call client "createrawtransaction" configuration


-- | Signs a raw transaction with default parameters.
sign :: T.Client                   -- ^ Our client context
     -> Btc.Transaction            -- ^ The transaction to sign
     -> Maybe [UnspentTransaction] -- ^ Previous outputs being spent by this transaction
     -> Maybe [RT.PrivateKey]      -- ^ Private keys to use for signing.
     -> IO Btc.Transaction         -- ^ The signed transaction
sign client tx utxs pks =
  let configuration = (configurationPks pks . configurationUtxs utxs  . configurationTx tx)  []

      configurationTx tx' c =
        c ++ [toJSON (Btc.encode tx')]

      configurationUtxs Nothing c = c
      configurationUtxs (Just utxs') c =
        c ++ [toJSON (map utxToDependency utxs')]

        where
          utxToDependency utx = object [
            ("txid",         toJSON (utx ^. transactionId)),
            ("vout",         toJSON (utx ^. vout)),
            ("scriptPubKey", toJSON (utx ^. scriptPubKey)),
            ("redeemScript", toJSON (utx ^. redeemScript))]


      configurationPks Nothing c = c
      configurationPks (Just privateKeys) c =
        c ++ [toJSON privateKeys]

      call :: IO Value
      call = I.call client "signrawtransaction" configuration

  in do
    res <- call

    case (res ^? key "hex" . _JSON) of
     Nothing -> error "Incorrect JSON response"
     Just hs -> (return . Btc.decode) hs
