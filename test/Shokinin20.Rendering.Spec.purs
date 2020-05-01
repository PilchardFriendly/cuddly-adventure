module Shokinin20.Rendering.Spec where

import Prelude

import Data.Array (range, replicate) as Array
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (empty, fromFoldable, member) as Set
import Data.String (joinWith)
import Data.String.Utils (stripMargin)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Shokinin20 (parseMap, renderMap, renderMap')
import Shokinin20.ViaFrontier (frontierOne, frontierSucc, renderFrontier)
import Shokinin20.Internal (allLocations, topY)
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
          describe "frontier" let
            renderer i = renderFrontier emptyMap (fst $ unwrap i)
            in do
            describe "emptyMap"
              let 
                input = frontierOne (5 /\ topY) 
                input2 = frontierSucc (\l -> Set.member l emptyMap) input
                input3 = frontierSucc (\l -> Set.member l emptyMap) input2
              in do
                it "depth 1 renders" do
                  
                  (renderMap' (renderer input) emptyMap) `shouldEqual` stripMargin """|..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |.....1...."""
                it "depth 2 renders" do
                  
                  (renderMap' (renderer input2) emptyMap) `shouldEqual` stripMargin """|..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |.....2....
                                                                                      |....212..."""
                        
                it "depth 3 renders" do
                  (renderMap' (renderer input3) emptyMap) `shouldEqual` stripMargin """|..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |..........
                                                                                      |.....3....
                                                                                      |....323...
                                                                                      |...32123.."""
