module Shokinin20.Spec (spec) where

import Prelude
import Control.Monad.Gen (class MonadGen, choose, chooseInt, elements, resize, unfoldable)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Graph as Graph
import Data.List (List)
import Data.List (fromFoldable) as List
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set (delete, empty, fromFoldable) as Set
import Data.String.Utils (stripMargin)
import Data.Tuple (Tuple(..), snd)
import Shokinin20.Internal (allLocations, topX, topY)
import Shokinin20
  ( Office(..)
  , Location
  , experiment
  , genStartingColumn
  , harness
  , office
  , officeGraph
  , officeHasPath
  , parseMap
  , possibleNeighbours
  , renderMap
  )
import Teletype (writeTeletype)
import Test.QuickCheck (class Arbitrary, mkSeed, (<?>))
import Test.QuickCheck.Gen (evalGen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck)

{-- Creates 9 random spots on the map, not including the back row--}
noiseGen :: forall m. MonadRec m => MonadGen m => m (List Location)
noiseGen = resize (\size -> min size 9) $ unfoldable locationGen
  where
  locationGen = do
    x <- chooseInt 0 topX
    y <- chooseInt 0 (topY - 1)
    pure $ Tuple x y

{-- New type for bounded testing --}
newtype Solvable
  = Solvable Office

derive instance newtypeSolvable :: Newtype Solvable _

instance arbitrarySolvable :: Arbitrary Solvable where
  arbitrary = solvableGen

solvableGen :: forall m. MonadRec m => MonadGen m => m Solvable
solvableGen = do
  noise <- noiseGen
  startingColumn <- genStartingColumn
  pure $ wrap $ office (withFewerSpaces noise) startingColumn
  where
  withFewerSpaces = foldr Set.delete (Set.fromFoldable allLocations)

newtype Unsolvable
  = Unsolvable Office

derive instance newtypeUnsolvable :: Newtype Unsolvable _

instance arbitraryUnsolvable :: Arbitrary Unsolvable where
  arbitrary = unsolvableGen

noisyUnsolvableGen :: forall m. MonadRec m => MonadGen m => m Unsolvable
noisyUnsolvableGen = do
  noise <- noiseGen
  startingColumn <- genStartingColumn
  pure $ wrap $ office (withMoreSpaces noise) startingColumn
  where
  withMoreSpaces = Set.fromFoldable

knownUnsolvableGen :: forall m. MonadRec m => MonadGen m => m Unsolvable
knownUnsolvableGen = do
  startingColumn <- genStartingColumn
  known <- parseMap <$> elements knownBadPatterns
  case known of
    Right spaces -> pure $ wrap $ office spaces startingColumn
    Left _ -> noisyUnsolvableGen

unsolvableGen :: forall m. MonadRec m => MonadGen m => m Unsolvable
unsolvableGen = choose knownUnsolvableGen noisyUnsolvableGen

knownBadPatterns :: (NonEmpty Array) String
knownBadPatterns =
  stripMargin
    <$> """  |O.........
                                        |.O........
                                        |..O.......
                                        |...O......
                                        |....O.....
                                        |.....O....
                                        |......O...
                                        |.......O..
                                        |........OO
                                        |.........."""
    :| []

spec :: Spec Unit
spec =
  describe "Shokinin 20"
    let
      seed = { newSeed: mkSeed 0, size: 1 }

      emptyMap = Set.fromFoldable allLocations

      fullMap = Set.empty
    in
      do
        describe "experiment" do
          describe "solvable"
            let
              experimentalRun = experiment (wrap 0.0) 100
            in
              do
                it "should report solvable maps as 100% likely to succeed" do
                  evalGen experimentalRun seed `shouldEqual` (wrap 1.0)
          describe "mostly solvable"
            let
              experimentalRun = experiment (wrap 0.20) 100
            in
              do
                it "should report mostly solvable maps as 80% <> 99% likely to succeed" do
                  evalGen experimentalRun seed `shouldSatisfy` (unwrap >>> \p -> p < 1.0 && p > 0.8)
          describe "unsolvable"
            let
              experimentalRun = experiment (wrap 1.0) 100
            in
              do
                it "should report unsolvable maps as 0% likely to succeed" do
                  evalGen experimentalRun seed `shouldEqual` (wrap 0.0)
        describe "harness" do
          describe "mock experiment"
            let
              subject = harness (mkSeed 0) [ wrap 0.0 ] 100 (\bias samples -> pure $ wrap 0.5)
            in
              do
                it "should display results" do
                  (snd $ writeTeletype subject) `shouldEqual` [ "Number of samples for each p: 100", "0.0 0.5" ]
          describe "mock triple experiment"
            let
              subject = harness (mkSeed 0) (wrap <$> [ 0.0, 0.1, 0.8 ]) 50 (\bias samples -> pure $ bias)
            in
              do
                it "should display results" do
                  (snd $ writeTeletype subject) `shouldEqual` [ "Number of samples for each p: 50", "0.0 0.0", "0.1 0.1", "0.8 0.8" ]
        describe "pathfinding"
          let
            emptyOffice = office emptyMap 5

            fullOffice = office fullMap 1
          in
            do
              it "empty map" do
                officeHasPath emptyOffice `shouldEqual` true
              it "full map" do
                officeHasPath fullOffice `shouldEqual` false
              describe "properties" do
                it "solvable maps always have solutions" do
                  quickCheck \(Solvable o@(Office { officeSpaces })) -> officeHasPath o <?> ("Failed for:\n" <> renderMap officeSpaces)
                it "unsolvable maps never have solutions" do
                  quickCheck \(Unsolvable o@(Office { officeSpaces })) -> (not $ officeHasPath o) <?> ("Failed for:\n" <> renderMap officeSpaces)
              describe "officeGraph" do
                describe "2 pt space (A<->B)"
                  let
                    ptA = Tuple 1 1

                    ptB = Tuple 1 0

                    input = Set.fromFoldable $ [ ptA, ptB ]

                    actual = officeGraph input

                    expected =
                      ( Graph.insertEdge ptA ptB 1
                          <<< Graph.insertEdge ptB ptA 1
                          <<< Graph.insertVertex ptA
                          <<< Graph.insertVertex ptB
                      )
                        Graph.empty
                  in
                    do
                      it "should have 2 vertices" do
                        Graph.vertices actual `shouldEqual` Graph.vertices expected
                      it "should have A->B edges" do
                        (Graph.isAdjacent ptA ptB actual) `shouldEqual` Graph.isAdjacent ptA ptB expected
                      it "should have B->A edges" do
                        (Graph.isAdjacent ptA ptB actual) `shouldEqual` Graph.isAdjacent ptA ptB expected
              describe "possible neighbours" do
                describe "single point"
                  let
                    pt = Tuple 1 10
                  in
                    do
                      it "should have 4 possibleNeighbours" do
                        possibleNeighbours pt `shouldEqual` List.fromFoldable [ Tuple 2 10, Tuple 0 10, Tuple 1 11, Tuple 1 9 ]

