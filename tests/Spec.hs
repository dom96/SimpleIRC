module Main (main) where

import           Test.Hspec.Monadic
import qualified CoreSpec

main :: IO ()
main = hspecX $ do
  describe "Core" CoreSpec.spec
