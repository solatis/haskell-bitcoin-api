module Network.Bitcoin.ClientSpec where

import qualified Data.Text as T ( pack )

import           Network.Bitcoin.Client
import           Test.Hspec

import qualified Network.Bitcoin.Rpc.Misc as Misc

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 18332 (T.pack "user") (T.pack "pass")

spec :: Spec
spec = do
  describe "when creating a new client session" $ do
    it "callback returns generated value" $ do
      testClient (\_ -> return "foo") `shouldReturn` "foo"

  describe "when testing miscelaneous functions" $ do
   it "should be able to return server info" $ do
     r <- testClient Misc.getInfo

     Misc.bitcoinVersion r `shouldBe` 100000
     Misc.bitcoindErrors r `shouldBe` (T.pack "")
