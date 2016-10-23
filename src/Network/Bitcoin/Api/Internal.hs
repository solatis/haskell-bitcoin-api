{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Internal where

import           Control.Lens              ((^.))
import           Control.Monad             (mzero)
import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

import           Data.Aeson
import qualified Data.HashMap.Strict       as HM

import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Network.Bitcoin.Api.Types as T

data RpcResult a = RpcResultError RpcError
                 | RpcResultOk a
  deriving (Show)

data RpcError = RpcError { errCode :: Int, errMsg :: T.Text }
    deriving (Show)

instance FromJSON a => FromJSON (RpcResult a) where
  parseJSON (Object o) =
    let checkError :: Bool
        checkError = HM.member "error" o && HM.lookup "error" o /= Just Null

        parseResult hasError o'
          | hasError  = RpcResultError <$> o' .: "error"
          | otherwise = RpcResultOk    <$> o' .: "result"

    in parseResult checkError o

  parseJSON _ = mzero

instance FromJSON RpcError where
  parseJSON (Object o) =
    RpcError <$> o .: "code" <*> o .: "message"
  parseJSON _ = mzero


call :: ( ToJSON a
        , FromJSON b
        , Show b )
     => T.Client -- ^ Our client context
     -> String   -- ^ The command we wish to execute
     -> a        -- ^ The parameters we wish to provide
     -> IO b     -- ^ The result that was returned
call client method params =
  let command = object [ "jsonrpc" .= T.pack "2.0"
                       , "method"  .= T.pack method
                       , "params"  .= params
                       , "id"      .= (1 :: Int)]

      call' = do
        r <- W.asJSON =<< WS.postWith
                (T.clientOpts client)
                (T.clientSession client)
                (T.clientUrl client)
                command
        return (r ^. W.responseBody)

  in do
    res <- call'

    case res of
     (RpcResultError err) -> fail ("An error occured: " ++ show err)
     (RpcResultOk obj) -> return obj

callRaw :: ( ToJSON a )
        => T.Client -- ^ Our client context
        -> String   -- ^ The command we wish to execute
        -> a        -- ^ The parameters we wish to provide
        -> IO BL.ByteString -- ^ Raw JSON response from bitcoind
callRaw client method params = do
    r <- WS.postWith
            (T.clientOpts client)
            (T.clientSession client)
            (T.clientUrl client)
            command
    return (r ^. W.responseBody)
    where command = object [ "jsonrpc" .= T.pack "2.0"
                           , "method"  .= T.pack method
                           , "params"  .= params
                           , "id"      .= (1 :: Int)]
