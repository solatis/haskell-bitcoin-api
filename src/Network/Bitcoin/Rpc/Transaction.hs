{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Transaction where

import           Data.Aeson

import           Control.Lens                                 ((^.))

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

      outToAddress (addr, btc) = (addr, (toJSON btc))

  in (return . Btc.decode) =<< I.call client "createrawtransaction" configuration
