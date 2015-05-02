{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Blockchain where

import           Data.Aeson
import           Data.Aeson.Types             (emptyArray)

import qualified Data.HexString               as HS
import qualified Data.Bitcoin.Block           as Btc

import qualified Data.Bitcoin.Types           as BT
import qualified Network.Bitcoin.Rpc.Internal as I
import qualified Network.Bitcoin.Rpc.Types    as T

getBlockCount :: T.Client -> IO Integer
getBlockCount client =
  I.call client "getblockcount" emptyArray

getBlockHash :: T.Client -> Integer -> IO BT.BlockHash
getBlockHash client offset =
  let configuration = [toJSON offset]

  in I.call client "getblockhash"  configuration

getBlock :: T.Client -> HS.HexString -> IO Btc.Block
getBlock client hash =
  let configuration = [toJSON hash, toJSON False]

  in (return . Btc.decode) =<< I.call client "getblock" configuration
