{-# LANGUAGE OverloadedStrings #-}

module Network.Bitcoin.Api.Internal
( RpcResult(..)
, RpcError(..)
, callRaw
, call
, callMaybe
)
where

import           Control.Lens              ((^.))
import           Control.Monad             (mzero)
import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

import qualified Control.Monad.Catch       as E
import           Network.HTTP.Client       (HttpException (..), HttpExceptionContent (..))

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
instance E.Exception RpcError

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

-- | Throws 'RpcError' exception on 'StatusCodeException'
call :: ( ToJSON a
        , FromJSON b
        )
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
        rE <- E.try $ W.asJSON =<< WS.postWith
                (T.clientOpts client)
                (T.clientSession client)
                (T.clientUrl client)
                command
        case rE of
          Right r -> return (r ^. W.responseBody)
          Left ex@(StatusCodeException _ hdrL _) ->
            throwNothing (hdrM >>= decode . BL.fromStrict)
                where hdrM = lookup "X-Response-Body-Start" hdrL
                      throwNothing = maybe (E.throwM (ex :: HttpException)) return
          Left ex -> E.throwM (ex :: HttpException)

  in do
    res <- call'

    case res of
     (RpcResultError err) -> E.throwM err
     (RpcResultOk obj) -> return obj

-- | Same as 'call' but return Nothing on not found, and Just on found
callMaybe :: ( ToJSON a
             , FromJSON b
             )
          => T.Client -> String -> a -> IO (Maybe b)
callMaybe c s p = catchNotFound $ call c s p

catchNotFound :: IO a -> IO (Maybe a)
catchNotFound cmd = do
    resE <- E.try cmd
    case resE of
      Right res -> return $ Just res
      Left  err -> if errCode err == -5 then return Nothing else E.throwM err

{-
StatusCodeException
  (Status {statusCode = 500, statusMessage = "Internal Server Error"})
  [ ("Content-Type","application/json")
    ,("Date","Mon, 28 Nov 2016 05:01:32 GMT")
    ,("Content-Length","98")
    , ( "X-Response-Body-Start",
        "{\"result\":null,\"error\":{\"code\":-5,\"message\":\"No information available about transaction\"},\"id\":1}\n"
      )
    ,("X-Request-URL","POST http://127.0.0.1:8332/")
  ]
  (CJ {expose = []})
-}
