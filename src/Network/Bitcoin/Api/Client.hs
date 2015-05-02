{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Rpc.Client ( T.Client (..)
                                  , withClient ) where

import           Control.Lens              ((&), (?~))
import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

import qualified Network.Bitcoin.Rpc.Types as T

-- | Initializes a client and prepares it for making requests against the
--   Bitcoin RPC API. Connection reuse is provided, and cleanup of any acquired
--   resources is handled automatically.
withClient :: String -> Int -> T.Text -> T.Text -> (T.Client -> IO a) -> IO a
withClient host port user pass callback =
  let options :: W.Options
      options = W.defaults & applyAuth

      applyAuth = W.auth ?~ W.basicAuth (TE.encodeUtf8 user) (TE.encodeUtf8 pass)

      generateUrl :: String
      generateUrl = "http://" ++ host ++ ":" ++ show port

  in WS.withSession (callback . T.Client generateUrl options)
