{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Bitcoin.Api.Types.TxInfo where

-- import           Control.Applicative ((<$>), (<*>))
-- import           Control.Lens.TH     (makeLenses)
import           Control.Monad       (mzero)

import qualified Data.Base58String   as B58S
import           Data.Word           (Word32, Word64)

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Bitcoin.Types  as BT
-- import qualified Data.Bitcoin.Transaction as Tx

-- import qualified Data.Text           as T


data TxInfo = TxInfo {
   txid        :: BT.TransactionId
  ,vins        :: [Vin]
  ,vouts       :: [Vout]
  ,confs       :: Integer
  ,blockhash   :: BT.BlockHash
  ,timestamp   :: Word64
} deriving (Eq, Show)


instance FromJSON TxInfo where
  parseJSON (Object o) =
    TxInfo
      <$> o .: "txid"
      <*> o .: "vin"
      <*> o .: "vout"
      <*> o .: "confirmations"
      <*> o .: "blockhash"
      <*> o .: "time"
  parseJSON _          = mzero


data Vout = Vout {
   amount      :: BT.Btc
  ,index       :: Word32
  ,addresses   :: [B58S.Base58String]
} deriving (Eq, Show)

parseScriptPubKey :: Value -> Parser [B58S.Base58String]
parseScriptPubKey (Object o) = o .: "addresses"
parseScriptPubKey _ = mzero

instance FromJSON Vout where
  parseJSON (Object o) =
    Vout
      <$> o .:  "value"
      <*> o .:  "n"
      <*> ( (o .:  "scriptPubKey") >>= parseScriptPubKey )
  parseJSON _          = mzero

data Vin = Vin {
    ref_txid        :: BT.TransactionId
   ,ref_index       :: Word32
} deriving (Eq, Show)

instance FromJSON Vin where
    parseJSON (Object o) =
        Vin
            <$> o .: "txid"
            <*> o .: "vout"
    parseJSON _          = mzero

