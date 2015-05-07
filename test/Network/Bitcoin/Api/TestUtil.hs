module Network.Bitcoin.Api.TestUtil (testClient, isStatusCodeException) where

import qualified Data.Bitcoin.Script                          as Btc
import qualified Data.Bitcoin.Transaction                     as Btc
import qualified Data.List                                    as L (find)
import           Data.Maybe                                   (isJust, mapMaybe)

import qualified Data.Text                                    as T (pack)
import           Control.Lens                                 ((^.))

import           Network.HTTP.Client                          (HttpException (..))

import           Network.Bitcoin.Api.Client

import qualified Network.Bitcoin.Api.Blockchain               as Blockchain
import qualified Network.Bitcoin.Api.Dump                     as Dump
import qualified Network.Bitcoin.Api.Mining                   as Mining
import qualified Network.Bitcoin.Api.Misc                     as Misc
import qualified Network.Bitcoin.Api.Transaction              as Transaction
import           Network.Bitcoin.Api.Types.UnspentTransaction (address, amount)
import qualified Network.Bitcoin.Api.Wallet                   as Wallet
import           Network.Wreq.Lens                            (statusCode)

import           Test.Hspec

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 18332 (T.pack "user") (T.pack "pass")

isStatusCodeException :: Int -> HttpException -> Bool
isStatusCodeException code (StatusCodeException s _ _) = s ^. statusCode == code
isStatusCodeException _ _ = False
