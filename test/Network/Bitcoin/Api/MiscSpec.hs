module Network.Bitcoin.Api.MiscSpec where

import           Control.Lens                                 ((^.))
import qualified Data.Text                                    as T (pack)

import qualified Network.Bitcoin.Api.Misc                     as Misc
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Test.Hspec

spec :: Spec
spec = do
  describe "when testing miscelaneous functions" $ do
   it "should be able to return server info" $ do
     r <- testClient Misc.getInfo

     r ^. Misc.bitcoinVersion `shouldBe` 100100
     r ^. Misc.bitcoindErrors `shouldBe` (T.pack "")
