module Shokinin20.Rendering.Spec where

import Prelude

import Data.Array (range, replicate) as Array
import Data.Either (Either(..))
import Data.Set (empty, fromFoldable) as Set
import Data.String (joinWith)
import Data.String.Utils (stripMargin)
import Data.Tuple (Tuple(..))
import Shokinin20 (parseMap, renderMap)
import Shokinin20.Internal (allLocations)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldEndWith, shouldStartWith)


spec :: Spec Unit
spec =  describe "Shokinin20.Rendering" let
      emptyMap = Set.fromFoldable allLocations

      fullMap = Set.empty
      in do
          describe "empty map"
            let
              input = emptyMap

              actual = renderMap input

              expected = joinWith "\n" (Array.replicate 10 "..........")

              parsed = parseMap expected
            in
              do
                it "renderMap" do
                  actual `shouldEqual` expected
                it "parseMap" do
                  parsed `shouldEqual` Right input
          describe "full map"
            let
              input = fullMap

              actual = renderMap input

              parsed = parseMap actual
            in
              do
                it "renderMap" do
                  actual `shouldEqual` joinWith "\n" (Array.replicate 10 "OOOOOOOOOO")
                it "parseMap" do
                  parsed `shouldEqual` Right input
          describe "top half row"
            let
              input = Set.fromFoldable $ (flip Tuple) 0 <$> Array.range 0 4
            in
              do
                it "renders map" do
                  renderMap input `shouldStartWith` ".....OOOOO\nO"
                it "parses rendered map" do
                  (parseMap $ renderMap input) `shouldEqual` (Right input)
          describe "bottom half column"
            let
              input = Set.fromFoldable $ Tuple 9 <$> Array.range 5 9
            in
              do
                it "renders map" do
                  renderMap input
                    `shouldEndWith`
                      ( stripMargin
                          """ |O
                                                             |OOOOOOOOOO
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO."""
                      )
                it "parses rendered map" do
                  (parseMap $ renderMap input) `shouldEqual` (Right input)