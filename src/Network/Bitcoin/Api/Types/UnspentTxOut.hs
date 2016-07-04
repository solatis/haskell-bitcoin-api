{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Types.UnspentTxOut where

-- import           Control.Applicative ((<$>), (<*>))
-- import           Control.Lens.TH     (makeLenses)
import           Control.Monad       (mzero)

import qualified Data.Base58String   as B58S
import           Data.Word           (Word32, Word64)

import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Bitcoin.Types  as BT


data UnspentTxOut = UnspentTxOut {
   confs       :: Integer
  ,amount      :: BT.Btc
  ,addresses   :: [B58S.Base58String]
} deriving (Eq, Show)

instance FromJSON UnspentTxOut where
  parseJSON (Object o) =
    UnspentTxOut
      <$> o .: "confirmations"
      <*> o .: "value"
      <*> ( (o .:  "scriptPubKey") >>= (.: "addresses") )
  parseJSON _          = mzero
