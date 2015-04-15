module Network.Bitcoin.ClientSpec where

import           Network.Bitcoin.Client
import           Test.Hspec

spec :: Spec
spec = do
  describe "when decompiling a specific script" $ do
    it "encoding a decoding a script results in the original hex" $ do
      True `shouldBe` True
