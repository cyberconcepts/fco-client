module Fco.Client.ClientSpec (main, spec) where

import Test.Hspec
--import Test.QuickCheck

--import Data.Set (size)
import Data.HashMap.Strict (size)
import Fco.Client


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "dummy" $ do
    it "is just for demonstration" $ do
      "foo bar" `shouldBe` "foo bar"
  describe "load Pocket data" $ do
    --it "number of tags found is 104" $ do
    it "number of items found is 40" $ do
      d <- loadFromFile "test/data/pocketdata.json"
      items <- processValue d
      size items `shouldBe` 40
