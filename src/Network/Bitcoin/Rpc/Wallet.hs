{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Wallet where

import qualified Data.Text                as T
import Data.Aeson
import Data.Aeson.Types

import qualified Network.Bitcoin.Internal as I
import qualified Network.Bitcoin.Types    as T
import qualified Network.Bitcoin.Rpc.Types    as RT
import qualified Network.Bitcoin.Rpc.Types.UnspentTransaction as UnspentTransaction

-- | Lists unspent transaction with default parameters
listUnspent :: T.Client
            -> IO [UnspentTransaction.UnspentTransaction]
listUnspent = listUnspentWith 1 9999999

-- | Lists unspent transactions with configurable parameters
listUnspentWith :: Integer  -- ^ Minimum amount of confirmations needed. Defaults to 1.
                -> Integer  -- ^ Maximum amount of confirmations. Defaults to 9999999.
                -> T.Client -- ^ Our client context
                -> IO [UnspentTransaction.UnspentTransaction]
listUnspentWith confMin confMax client =
  let configuration = [toJSON confMin, toJSON confMax, emptyArray]

  in I.call client "listunspent" configuration
