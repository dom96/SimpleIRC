module CoreSpec (spec) where

import           Test.Hspec.Monadic

spec :: Specs
spec = do
describe "id" $ do
  it "returns the original argument" $
    id "foo" == "foo"
