module Network.Bitcoin.Api.TestUtil (testClient, isStatusCodeException) where

import qualified Data.Text                                    as T (pack)
import           Control.Lens                                 ((^.))
import           Network.HTTP.Client                          (HttpException (..)
                                                              ,HttpExceptionContent (..)
                                                              ,responseStatus)
import           Network.Wreq.Lens                            (statusCode)

import           Network.Bitcoin.Api.Client

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 18332 (T.pack "user") (T.pack "pass")

isStatusCodeException :: Int -> HttpException -> Bool
isStatusCodeException code (HttpExceptionRequest _ (StatusCodeException res _)) =
  responseStatus res ^. statusCode == code
isStatusCodeException _ _ = False
