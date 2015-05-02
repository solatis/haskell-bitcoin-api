{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Mining where

import           Data.Aeson

import qualified Data.Bitcoin.Block             as Btc

import qualified Network.Bitcoin.Rpc.Internal   as I
import qualified Network.Bitcoin.Rpc.Types      as T
import qualified Network.Bitcoin.Rpc.Blockchain as Blockchain

-- | Generate a certain amount of new blocks. Available in 'regtest' mode only.
generate :: T.Client       -- ^ Our client context
         -> Integer        -- ^ Amount of blocks to generate
         -> IO [Btc.Block] -- ^ The generated blocks
generate client blocks =
  let configuration = [toJSON True, toJSON blocks]
  in do
    hashes <- I.call client "setgenerate" configuration
    mapM (Blockchain.getBlock client) hashes
