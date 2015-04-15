module Network.Bitcoin.Types where

import qualified Network.Wreq          as W
import qualified Network.Wreq.Session  as WS

data Client = Client {
  clientUrl     :: String,    -- ^ The JSON RPC url
  clientOpts    :: W.Options, -- ^ Default HTTP options to use with `wreq` requests
  clientSession :: WS.Session -- ^ Connection reuse of our HTTP session
  } deriving ( Show )
