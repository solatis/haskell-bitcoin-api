{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Dump where

import           Data.Aeson.Types             (emptyArray)

import qualified Data.Bitcoin.Types           as BT

import qualified Network.Bitcoin.Rpc.Internal as I
import qualified Network.Bitcoin.Rpc.Types    as T

getPrivateKey :: T.Client -> BT.Address -> IO BT.PrivateKey
getPrivateKey client addr =
  let configuration = [addr]
  in I.call client "dumpprivkey" configuration
