{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Blockchain where

import           Data.Aeson
import           Data.Aeson.Types             (emptyArray)

import qualified Data.HexString               as HS
import qualified Data.Bitcoin.Block           as Btc

import qualified Data.Bitcoin.Types           as BT
import qualified Network.Bitcoin.Api.Internal as I
import qualified Network.Bitcoin.Api.Types    as T

-- | Gets the amount of blocks currently in the blockchain, also known as the
--   'height' of the blockchain.
getBlockCount :: T.Client -> IO Integer
getBlockCount client =
  I.call client "getblockcount" emptyArray

-- | Get the hash of a block based on its offset (height).
getBlockHash :: T.Client        -- ^ Our client context
             -> Integer         -- ^ The height/offset of the block. 0 is the genesis block.
             -> IO BT.BlockHash -- ^ The hash of the block
getBlockHash client offset =
  let configuration = [toJSON offset]

  in I.call client "getblockhash"  configuration

-- | Gets a block based on its hash.
getBlock :: T.Client     -- ^ Our session context
         -> HS.HexString -- ^ Hexadecimal representation of the hash of a block
         -> IO Btc.Block -- ^ The block
getBlock client hash =
  let configuration = [toJSON hash, toJSON False]

  in (return . Btc.decode) =<< I.call client "getblock" configuration
