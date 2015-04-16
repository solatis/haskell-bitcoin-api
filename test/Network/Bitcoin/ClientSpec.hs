{-# LANGUAGE LambdaCase #-}

module Network.Bitcoin.ClientSpec where

import qualified Data.Text                as T (pack)

import           Network.HTTP.Client      (HttpException (..))

import           Network.Wreq.Lens (statusCode)
import           Control.Lens ((^.))
import           Network.Bitcoin.Client
import qualified Network.Bitcoin.Rpc.Misc as Misc
import           Test.Hspec

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 18332 (T.pack "user") (T.pack "pass")

isStatusCodeException :: Int -> HttpException -> Bool
isStatusCodeException code (StatusCodeException s _ _) = s ^. statusCode == code
isStatusCodeException _ _ = False

spec :: Spec
spec = do
  describe "when creating a new client session" $ do
    it "callback returns generated value" $ do
      testClient (\_ -> return "foo") `shouldReturn` "foo"

    it "fails when providing invalid authentication credentials" $ do
      withClient "127.0.0.1" 18332 (T.pack "invaliduser") (T.pack "invalidpass") Misc.getInfo `shouldThrow` isStatusCodeException 401

  describe "when testing miscelaneous functions" $ do
   it "should be able to return server info" $ do
     r <- testClient Misc.getInfo

     r ^. Misc.bitcoinVersion `shouldBe` 100000
     r ^. Misc.bitcoindErrors `shouldBe` (T.pack "")
