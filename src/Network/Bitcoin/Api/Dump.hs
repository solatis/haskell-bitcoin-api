{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Dump where

import           Data.Aeson.Types             (emptyArray)

import qualified Data.Bitcoin.Types           as BT

import qualified Network.Bitcoin.Api.Internal as I
import qualified Network.Bitcoin.Api.Types    as T

getPrivateKey :: T.Client -> BT.Address -> IO BT.PrivateKey
getPrivateKey client addr =
  let configuration = [addr]
  in I.call client "dumpprivkey" configuration
