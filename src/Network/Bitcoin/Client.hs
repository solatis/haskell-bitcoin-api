{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Client ( T.Client (..)
                              , withClient ) where

import qualified Network.Wreq          as W
import qualified Network.Wreq.Session  as WS
import Control.Lens ((?~), (&))

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

import qualified Network.Bitcoin.Types as T

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
