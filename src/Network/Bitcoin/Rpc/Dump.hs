{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Dump where

import Data.Aeson.Types ( emptyArray )

import qualified Network.Bitcoin.Internal  as I
import qualified Network.Bitcoin.Rpc.Types as RT
import qualified Network.Bitcoin.Types     as T

getPrivateKey :: T.Client -> RT.Address -> IO RT.PrivateKey
getPrivateKey client addr =
  let configuration = [addr]
  in I.call client "dumpprivkey" configuration
