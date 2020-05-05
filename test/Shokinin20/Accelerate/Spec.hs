{-# LANGUAGE BlockArguments #-}
module Shokinin20.Accelerate.Spec (spec) where
import Prelude hiding (zipWith)
import Test.Hspec
import Test.Hspec.Expectations
import Data.Array.Accelerate

spec :: Spec
spec = describe "dotp" do
    it "should calculate dor product"
        pending    


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = fold (+) 0 (zipWith (*) xs ys)