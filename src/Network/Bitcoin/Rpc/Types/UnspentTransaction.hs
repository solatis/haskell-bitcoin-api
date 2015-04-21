{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Bitcoin.Rpc.Types.UnspentTransaction where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (mzero)

import           Data.Word                 (Word64)

import           Data.Aeson
import           Data.Aeson.Types

import qualified Network.Bitcoin.Rpc.Types as RT

import qualified Data.Text                 as T

-- | A transaction that is not yet spent. Every output transaction
--   relies on one or more unspent input transansactions.
data UnspentTransaction = UnspentTransaction {
  _amount        :: Word64,

  -- | Transaction identifier to uniquely identify this transaction.
  _transactionId :: RT.TransactionId,

  _vout          :: Integer,

  _spendable     :: Bool,

  _address       :: T.Text,

  -- | The amount of confirmations this transaction has
  _confirmations :: Integer,

  _scriptPubKey  :: T.Text

  } deriving ( Show )

makeLenses ''UnspentTransaction

instance FromJSON UnspentTransaction where
  parseJSON (Object o) =
    UnspentTransaction
      <$> o .:  "amount"
      <*> o .:  "txid"
      <*> o .:  "vout"
      <*> o .:  "spendable"
      <*> o .:  "address"
      <*> o .:  "confirmations"
      <*> o .:  "scriptPubKey"
  parseJSON _          = mzero
