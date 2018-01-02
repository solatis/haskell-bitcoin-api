{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Bitcoin.Api.Misc
( module Network.Bitcoin.Api.Misc
, I.callRaw
, I.RpcError(..)
)
where

import           Control.Applicative          ((<$>), (<*>))
import           Control.Lens.TH              (makeLenses)
import           Control.Monad                (mzero)

import           Data.Aeson
import           Data.Aeson.Types             (emptyArray)
import qualified Data.Text                    as T

import qualified Network.Bitcoin.Api.Internal as I
import qualified Network.Bitcoin.Api.Types    as T
import qualified Data.Bitcoin.Types           as BT

data BitcoinInfo = BitcoinInfo {

  -- | What version of bitcoind are we running?
  _bitcoinVersion       :: Integer,

  -- | What is bitcoind's current protocol number?
  _protocolVersion      :: Integer,

  -- | What version is the wallet?
  _walletVersion        :: Integer,

  -- | How much money is currently in the wallet?
  _balance              :: BT.Btc,

  -- | The number of blocks in our chain.
  _numBlocks            :: Integer,

  -- | How many peers are we connected to?
  _numConnections       :: Integer,

  -- | A blank string if we're not using a proxy.
  _proxy                :: T.Text,

  -- | The difficulty multiplier for bitcoin mining operations.
  _generationDifficulty :: Double,

  -- | Are we on the test network (as opposed to the primary
  --   bitcoin network)?
  _onTestNetwork        :: Bool,

  -- | The timestamp of the oldest key in the key pool.
  _keyPoolOldest        :: Integer,

  -- | The size of the key pool.
  _keyPoolSize          :: Integer,

  -- | How much do we currently pay as a transaction fee?
  _transactionFeePaid   :: Integer,

  -- | If the wallet is unlocked, the number of seconds until a
  --   re-lock is needed.
  _unlockedUntil        :: Maybe Integer,

  -- | Any alerts will show up here. This should normally be an
  --   empty string.
  _bitcoindErrors       :: T.Text

  } deriving ( Show )

makeLenses ''BitcoinInfo

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

-- | Estimate transaction fee (in bitcoins per kilobyte) needed to
--    get a transaction confirmed within specified number of blocks.
--   Note: only works for num_blocks >= 2
estimateFee :: T.Client -> Integer -> IO BT.Btc
estimateFee client maxBlocks =
  -- Block number >= 2 && <= 25
  let capMaxBlks rawNum = max 2 (min 25 rawNum) in
  I.call client "estimatefee" [toJSON $ capMaxBlks maxBlocks]

