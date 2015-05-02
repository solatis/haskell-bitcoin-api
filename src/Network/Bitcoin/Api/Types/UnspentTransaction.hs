{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Bitcoin.Api.Types.UnspentTransaction where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens.TH     (makeLenses)
import           Control.Monad       (mzero)

import qualified Data.Base58String   as B58S
import           Data.Word           (Word64)

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Bitcoin.Types  as BT

import qualified Data.Text           as T

-- | A transaction that is not yet spent. Every output transaction
--   relies on one or more unspent input transansactions.
--
--   For more detailed documentation of the fields, see:
--     https://bitcoin.org/en/developer-reference#listunspent
data UnspentTransaction = UnspentTransaction {

  -- | The transaction amount in 'BT.Btc'
  _amount        :: BT.Btc,

  -- | Transaction identifier to uniquely identify the transaction.
  _transactionId :: BT.TransactionId,

  -- | The index of the output of the transaction that has been spent.
  _vout          :: Integer,

  -- | Whether this input is spendable. If not, it means it is an output
  --   of a watch-only address.
  _spendable     :: Bool,

  -- | The P2PKH or P2SH address this transaction belongs to. Only available in
  --   case of P2PKH or P2SH output scripts.
  _address       :: Maybe B58S.Base58String,

  -- | If the address belongs to an account, the account is returned.
  _account       :: Maybe T.Text,

  -- | The amount of confirmations this transaction has
  _confirmations :: Integer,

  -- | The output script paid, encoded as hex
  _scriptPubKey  :: T.Text,

  -- | If the output is a P2SH whose script belongs to this wallet, this is the
  --   redeem script.
  _redeemScript  :: Maybe T.Text

  } deriving ( Show )

makeLenses ''UnspentTransaction

instance FromJSON UnspentTransaction where
  parseJSON (Object o) =
    UnspentTransaction
      <$> o .:  "amount"
      <*> o .:  "txid"
      <*> o .:  "vout"
      <*> o .:  "spendable"
      <*> o .:? "address"
      <*> o .:? "account"
      <*> o .:  "confirmations"
      <*> o .:  "scriptPubKey"
      <*> o .:? "redeemScript"

  parseJSON _          = mzero
