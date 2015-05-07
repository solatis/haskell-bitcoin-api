module Network.Bitcoin.Api.ClientSpec where

import qualified Data.Text                                    as T (pack)

import           Network.Bitcoin.Api.Client

import qualified Network.Bitcoin.Api.Misc                     as Misc

import           Network.Bitcoin.Api.TestUtil                 (isStatusCodeException,
                                                               testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when creating a new client session" $ do
    it "callback returns generated value" $ do
      testClient (\_ -> return "foo") `shouldReturn` "foo"

    it "fails when providing invalid authentication credentials" $ do
      withClient "127.0.0.1" 18332 (T.pack "invaliduser") (T.pack "invalidpass") Misc.getInfo `shouldThrow` isStatusCodeException 401
