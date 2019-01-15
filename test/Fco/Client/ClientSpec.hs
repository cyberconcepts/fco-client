module Fco.Client.ClientSpec (main, spec) where

import Test.Hspec
--import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import Fco.Client.Pocket


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
    it "items found: 40, tags: 104" $ do
      value <- loadFromFile "test/data/pocketdata.json"
      let items = extractData $ extractLinkList value
      HM.size items `shouldBe` 40
      Set.size (collectTags items) `shouldBe` 104
