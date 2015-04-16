{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Misc where

import Control.Monad (mzero)
import qualified Data.Text                as T

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types ( emptyArray )

import qualified Network.Bitcoin.Internal as I
import qualified Network.Bitcoin.Types    as T

data BitcoinInfo = BitcoinInfo {
  -- | What version of bitcoind are we running?
  bitcoinVersion :: Integer,

  -- | What is bitcoind's current protocol number?
  protocolVersion :: Integer,

  -- | What version is the wallet?
  walletVersion :: Integer,

  -- | How much money is currently in the wallet?
  balance :: Integer,

  -- | The number of blocks in our chain.
  numBlocks :: Integer,

  -- | How many peers are we connected to?
  numConnections :: Integer,

  -- | A blank string if we're not using a proxy.
  proxy :: T.Text,

  -- | The difficulty multiplier for bitcoin mining operations.
  generationDifficulty :: Double,

  -- | Are we on the test network (as opposed to the primary
  --   bitcoin network)?
  onTestNetwork :: Bool,

  -- | The timestamp of the oldest key in the key pool.
  keyPoolOldest :: Integer,

  -- | The size of the key pool.
  keyPoolSize :: Integer,

  -- | How much do we currently pay as a transaction fee?
  transactionFeePaid :: Integer,

  -- | If the wallet is unlocked, the number of seconds until a
  --   re-lock is needed.
  unlockedUntil :: Maybe Integer,

  -- | Any alerts will show up here. This should normally be an
  --   empty string.
  bitcoindErrors :: T.Text

  } deriving ( Show )

instance FromJSON BitcoinInfo where
  parseJSON (Object o) =
    BitcoinInfo
      <$> o .:  "version"
      <*> o .:  "protocolversion"
      <*> o .:  "walletversion"
      <*> o .:  "balance"
      <*> o .:  "blocks"
      <*> o .:  "connections"
      <*> o .:  "proxy"
      <*> o .:  "difficulty"
      <*> o .:  "testnet"
      <*> o .:  "keypoololdest"
      <*> o .:  "keypoolsize"
      <*> o .:  "paytxfee"
      <*> o .:? "unlocked_until"
      <*> o .:  "errors"
  parseJSON _          = mzero

getInfo :: T.Client -> IO BitcoinInfo
getInfo client =
  I.call client "getinfo" emptyArray
