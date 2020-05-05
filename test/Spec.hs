{-# LANGUAGE BlockArguments #-}
import Test.Hspec

import qualified Shokinin20.Spec (spec)
import qualified Shokinin20.Accelerate.Spec (spec)
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "all" do
            describe "Shokinin20"                Shokinin20.Spec.spec
            describe "Shokinin20.Accelerate"     Shokinin20.Accelerate.Spec.spec