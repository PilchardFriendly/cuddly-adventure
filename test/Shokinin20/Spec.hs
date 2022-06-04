{-# LANGUAGE RankNTypes, MultiParamTypeClasses,AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Shokinin20.Spec (spec) where

import Prelude
import Control.Arrow ((>>>))
import Control.Monad.Writer (Writer)
import Control.Newtype (Newtype, pack, unpack)

import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Hedgehog.Internal.Gen (MonadGen, evalGen, evalGenT)
import Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Tree (treeValue, runTreeT, runTree)
import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.Hedgehog    (Gen, PropertyT, assert, diff, forAll, hedgehog, forAllWith, (/==), (===))

import Data.Set (Set)
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set

import Polysemy
import Teletype (Teletype, runTeletypePure)
import Text.Megaparsec ((<?>))
-- import HaskellWorks.Hspec.Hedgehog (require, requireProperty)

import Shokinin20.Shokinin20 (harness, experiment, office, genStartingColumn)
import Shokinin20.Types (Location, Office(..), Probability, Boolean, HasOfficePath(..))
import Shokinin20.Internal (topX, topY, possibleNeighbours, allLocations)
import Shokinin20.ViaGraph (ViaGraph(..), OfficeGraph, officeGraph, officeHasPath)
import Shokinin20.ViaFrontier (ViaFrontier(..), Frontier)
import Shokinin20.Rendering (renderMap, parseMap, renderOffice)


emptyGraph :: Graph
emptyGraph  = Graph.buildG (topX,topY) []

newtype ViaBacktracking a = ViaBacktracking a
instance HasOfficePath ViaBacktracking where
    extractHasPath _ = False
    calculateHasPath _ = ViaBacktracking False

{-- Creates 9 random spots on the map, not including the back row--}
noiseGen :: MonadGen m => m [Location]
noiseGen = Gen.list (Range.linear 0 9) locationGen

locationGen :: MonadGen m => m Location
locationGen = do
      x <- Gen.int (Range.linear 0 topX)
      y <- Gen.int (Range.linear 0 (topY - 1))
      pure (x,y)

{-- New type for bounded testing --}
newtype Solvable
  = Solvable Office
instance Newtype Solvable Office

solvableGen :: forall m. MonadGen m => m Solvable
solvableGen = do
  noise <- noiseGen
  pack . office (withFewerSpaces noise) <$> genStartingColumn
  where
  withFewerSpaces = foldr Set.delete (Set.fromList allLocations)

newtype Unsolvable
  = Unsolvable Office
instance Newtype Unsolvable Office


noisyUnsolvableGen :: forall m. MonadGen m => m Unsolvable
noisyUnsolvableGen = do
  noise <- noiseGen
  pack . office (withMoreSpaces noise) <$> genStartingColumn
  where
    withMoreSpaces = Set.fromList

knownUnsolvableGen :: forall m. MonadGen m => m Unsolvable
knownUnsolvableGen = do
  startingColumn <- genStartingColumn
  known <- parseMap <$> Gen.element knownBadPatterns
  case known of
    Right spaces -> pure $ pack $ office spaces startingColumn
    Left _ -> noisyUnsolvableGen

unsolvableGen :: forall m. MonadGen m => m Unsolvable
unsolvableGen = Gen.choice [knownUnsolvableGen,noisyUnsolvableGen]

knownBadPatterns :: [String]
knownBadPatterns =
  [unlines
    ["O........."
    ,".O........"
    ,"..O......."
    ,"...O......"
    ,"....O....."
    ,".....O...."
    ,"......O..."
    ,".......O.."
    ,"........OO"
    ,".........."]]





spec :: Spec
spec =
  describe "Shokinin 20"
    let
      seed = Seed 0 1

      emptyMap = Set.fromList allLocations

      fullMap = Set.empty
      solution1 :: Office -> ViaGraph Boolean
      solution1 = calculateHasPath
      solver1 = extractHasPath . solution1
      solution2 :: Office -> ViaBacktracking Boolean
      solution2 = calculateHasPath
      solution3 :: Office -> ViaFrontier Boolean
      solution3 = calculateHasPath

      evalExperiment :: Gen Probability -> Maybe Probability
      evalExperiment gen = treeValue <$> evalGen (Size 1000) (Seed.from 0) gen
    in
      do
        describe "experiment" $ do
          describe "solvable" $
            let
              experimentGen :: forall m. MonadGen m => m Probability
              experimentGen = experiment (pack 0.0) 100 solver1
            in
              do
                it "should report solvable maps as 100% likely to succeed" $
                  evalExperiment experimentGen `shouldBe` Just (pack 1.0)
          describe "mostly solvable" $
            let
              experimentGen = experiment (pack 0.2) 100 solver1
            in
              it "should report mostly solvable maps as 80% <> 99% likely to succeed" $
                  evalExperiment experimentGen  `shouldSatisfy` Prelude.maybe False ((\p -> p < 1.0 && p > 0.8) . unpack)
          describe "unsolvable"
            let
              experimentGen = experiment (pack 1.0) 100 solver1
            in
              do
                it "should report unsolvable maps as 0% likely to succeed" $
                  evalExperiment experimentGen `shouldBe` Just (pack 0.0)
        describe "harness" do
          describe "mock experiment"
            let
              subject = harness (Seed.from 0) [ pack 0.0 ] 100 solver1 (\bias samples solver -> pure $ pack 0.5)
            in
              it "should display results" $
                  (fst . run . runTeletypePure $ subject) `shouldBe` [ "Number of samples for each p: 100", "0.0 0.500" ]
          describe "mock triple experiment"
            let
              subject :: Member Teletype r => Sem r ()
              subject = harness (Seed.from 0) probabilities 50 solver1 (\bias samples solver -> pure bias)
              probabilities :: [Probability]
              probabilities = (pack <$> [ 0.0, 0.1, 0.8 ])
            in
              do
                it "should display results" $
                  (fst . run . runTeletypePure $ subject) `shouldBe` [ "Number of samples for each p: 50", "0.0 0.000", "0.1 0.100", "0.8 0.800" ]
        describe "pathfinding"
          let
            emptyOffice = office emptyMap 5

            fullOffice = office fullMap 1

            showOffice :: Newtype t Office => t -> String
            showOffice = renderOffice . unpack
          in
            do
              it "empty map" do
                officeHasPath emptyOffice `shouldBe` True
              it "full map" do
                officeHasPath fullOffice `shouldBe` False
              describe "properties" do
                it "solvable maps always have solutions" $ hedgehog do
                    o <- unpack <$> forAllWith  showOffice solvableGen
                    assert $ officeHasPath o

                it "solvable maps are solved by both graph and backtracking" do
                  pending
                  -- requireProperty $ do
                    --  Solvable o@(Office officeSpaces _ ) <- forAll solvableGen
                    --  (extractHasPath $ solution1 o)  === (extractHasPath $ solution2 o) 
                    -- <?> ("Mismatch between `graph` and `backtrack for:`" <> renderMap officeSpaces)
                it "solvable maps are solved by both graph and frontier" $ hedgehog do
                    o <- unpack <$> forAllWith showOffice solvableGen
                    extractHasPath (solution1 o)  === extractHasPath (solution3 o)

                it "unsolvable maps never have solutions" $ hedgehog do
                    o <- unpack <$> forAllWith showOffice noisyUnsolvableGen
                    assert (not $ officeHasPath o)

                it "unsolvable maps are solved by both graph and frontier" $ hedgehog $ do
                    o <- unpack <$> forAllWith showOffice unsolvableGen
                    extractHasPath (solution1 o)  === extractHasPath (solution3 o)

              describe "officeGraph" do
                describe "2 pt space (A<->B)"
                  let
                    ptA = (1,1)

                    ptB = (1,0)

                    input = Set.fromList [ ptA, ptB ]
                    (actual,_,_) = officeGraph input

                    (expected,_,_) = Graph.graphFromEdges [ (ptA, ptA, [ptB]), (ptB, ptB, [ptA])]
                  in
                    do
                      it "should have 2 vertices" do
                        Graph.vertices actual `shouldBe` Graph.vertices expected
                      it "should have A->B andedges" do
                        Graph.edges actual `shouldBe` [(0, 1), (1 ,0)]
                      -- it "should have B->A edges" do
                      --   (Graph.isAdjacent ptA ptB actual) `shouldBe` Graph.isAdjacent ptA ptB expected
              describe "possible neighbours" do
                describe "single point"
                  let
                    pt = (1,10)
                  in
                    do
                      it "should have 4 possibleNeighbours" do
                        possibleNeighbours pt `shouldBe` [ (2, 10), (0, 10), (1, 11), (1, 9 )]
              describe "Frontier" let
                  f0 :: Frontier
                  f0 = mempty
                  f1 :: Frontier
                  f1 = pack (Nothing,1)
                  f2 :: Frontier
                  f2 = pack (Just (1,2),4)
                  f3 :: Frontier
                  f3 = pack (Just (0,0),5)
                in do
                describe "monoid" do
                  it "0+0=0" do
                    ( f0 <> mempty ) `shouldBe` mempty
                  it "0+a=a" do
                    (f0 <> f1) `shouldBe` f1
                  it "a < b => a+b=a" do
                    (f1 <> f2) `shouldBe` f1
                  it "a > b => b+a=a" do
                    (f2 <> f1) `shouldBe` f1
