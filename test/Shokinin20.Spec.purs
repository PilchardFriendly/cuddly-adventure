module Shokinin20.Spec(spec) where

import Prelude

import Control.Biapply (biapply)
import Control.Monad.Gen (class MonadGen, chooseFloat, chooseInt)
import Data.Array (range, replicate, filter) as Array
import Data.Foldable (foldr, any)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List)
import Data.List (catMaybes, concatMap, fromFoldable, range) as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (empty, fromFoldable, insert, member, toUnfoldable) as Set
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldEndWith, shouldStartWith)

type Location = Tuple Int Int

data Office = Office {
    officeStartingColumn :: Int
    ,officeSpaces :: Set Location  -- keyed by tuple e.g. (1,5)  
}

office :: Set Location -> Int -> Office
office spaces startingColumn = Office { 
        officeStartingColumn: startingColumn
        ,officeSpaces: Set.insert (Tuple startingColumn topY) spaces
}

-- Probabilty is a float between 0 and 1

newtype Probability = Probability Number
instance boundedProbability :: Bounded Probability where
    top = wrap 1.0
    bottom = wrap 0.0
derive instance newtypeProbability :: Newtype Probability _
derive instance eqProbability :: Eq Probability
derive instance ordProbability :: Ord Probability

genMaybeP :: forall m a. MonadGen m => Probability -> a -> m (Maybe a)
genMaybeP p a = do
    toss <- chooseFloat 0.0 1.0
    pure if (toss < unwrap p) then Just a else Nothing

genObstacles :: forall m. MonadGen m => Probability -> List Location -> m (Set Location)
genObstacles prob xs = Set.fromFoldable <<< List.catMaybes <$> traverse sample xs
    where
        sample :: Location -> m (Maybe Location)
        sample = genMaybeP prob

genStartingColumn :: forall m. MonadGen m => m Int
genStartingColumn = chooseInt 0 topX

topY :: Int
topY = 9
topX :: Int
topX = 9

allLocations :: List Location
allLocations = do
    x <- List.range 0 topX
    y <- List.range 0 topY
    pure $ Tuple x y

renderLocation ::  Set Location -> Location -> String
renderLocation locations location = if (Set.member location locations) then "." else "O"
    

renderLine' :: (Location -> String)  -> Int -> Array Int -> String
renderLine' f y xs = joinWith "" do
    x <- xs
    pure $ f (Tuple x y)    

renderMap :: Set Location -> String
renderMap locations = renderMap' (renderLocation locations) locations

renderMap' ::  (Location -> String) -> Set Location -> String
renderMap' f locations = joinWith "\n" do
    y <- Array.range 0 topY
    pure $ renderLine' f y (Array.range 0 topX)

{-- officeHasPath: True if there is a path from (n,topY)-> (m, 0) where n is the starting position--}
officeHasPath :: Office -> Boolean
{-- Using connectedComponents is brute force - since it will find all possible paths, effectively.
    It gets cheaper the more full of obstacles the office becomes
    but it's a useful library --}
officeHasPath o@(Office { officeStartingColumn, officeSpaces}) = any containsStartAndEnd $ Graph.vertices <$> (Graph.connectedComponents $ officeGraph officeSpaces)
    where
        containsStartAndEnd :: List Location -> Boolean
        containsStartAndEnd locations = any isExit locations && any isStart locations
        isExit :: Location -> Boolean
        isExit t = 0 == snd t 
        isStart :: Location -> Boolean
        isStart t = t == Tuple officeStartingColumn topY

{-- looks up, down, left, and right--}
possibleNeighbours :: Location -> Array Location
possibleNeighbours l = sequence offsets l
    where
        inc :: Int -> Int
        inc = (+) 1
        dec :: Int -> Int
        dec = flip (-) 1
        offsets = biapply <$>[Tuple inc identity, Tuple dec identity, Tuple identity inc, Tuple identity dec]

{-- Turns a set of points into a di-graph based on cartesian adjacency (not diagonals)--}
officeGraph :: Set Location -> Graph Location Int
officeGraph spaces = buildAllEdges $ buildAllVertices $ Graph.empty
    where
        allVertices :: List Location
        allVertices = Set.toUnfoldable spaces
        allEdges :: List (Tuple Location Location)
        allEdges = List.concatMap neighbouringEdges allVertices
        isSpace :: Location -> Boolean
        isSpace = flip Set.member spaces
        {-- Only neighbours that are in the set of spaces count --}
        neighbouringEdges :: Location -> List (Tuple Location Location)
        neighbouringEdges l = List.fromFoldable $ Tuple l <$> (Array.filter isSpace $ possibleNeighbours l)

        
        buildAllEdges :: Graph Location Int -> Graph Location Int
        buildAllEdges g = foldr edge g allEdges
        edge :: Tuple Location Location -> Graph Location Int -> Graph Location Int
        edge t g = (uncurry Graph.insertEdge) t 1 g 
        buildAllVertices :: Graph Location Int -> Graph Location Int
        buildAllVertices g = foldr Graph.insertVertex g allVertices

spec :: Spec Unit
spec = describe "Shokinin 20" 
    let emptyMap = Set.fromFoldable allLocations
        fullMap = Set.empty in do

    describe "pathfinding" 
        let emptyOffice = office emptyMap 5
            fullOffice = office fullMap 1
        in do 
        it "empty map" do
            officeHasPath emptyOffice `shouldEqual` true
        it "full map" do
            officeHasPath fullOffice `shouldEqual` false

        describe "properties" do
            pending "empty maps will always have a path even if you take away 9 spaces "
            pending "full maps will never have a path even if you take away 9 obstacles"

        describe "officeGraph" do
            describe "2 pt space (A<->B)" let 
                ptA = Tuple 1 1
                ptB = Tuple 1 0
                input = Set.fromFoldable $ [ptA, ptB]
                actual = officeGraph input
                expected = (Graph.insertEdge ptA ptB 1
                    <<< Graph.insertEdge ptB ptA 1 
                    <<< Graph.insertVertex ptA
                    <<< Graph.insertVertex ptB) Graph.empty
                in do
                it "should have 2 allVertices" do
                    Graph.vertices actual `shouldEqual` Graph.vertices expected
                it "should have A->B edges" do
                    (Graph.isAdjacent ptA ptB actual) `shouldEqual` Graph.isAdjacent ptA ptB expected
                it "should have B->A edges" do
                    (Graph.isAdjacent ptA ptB actual) `shouldEqual` Graph.isAdjacent ptA ptB expected

        describe "neightbours" do
            describe "single point"  
                let
                    pt = Tuple 1 10
                in do
                    it "should have 4 possibleNeighbours" do
                        possibleNeighbours pt `shouldEqual`  [Tuple 2 10, Tuple 0 10, Tuple 1 11, Tuple 1 9]

    describe "rendering" do
        describe "empty map" 
            let input = emptyMap
                actual = renderMap input
            in do
                it "renderMap" do
                    actual `shouldEqual` joinWith "\n" (Array.replicate 10 "..........")

        describe "full map"
            let input = fullMap
                actual = renderMap input
            in do
                it "renderMap" do
                    actual `shouldEqual` joinWith "\n" (Array.replicate 10 "OOOOOOOOOO")

        describe "top half row"
            let input =  Set.fromFoldable $ (flip Tuple) 0 <$> Array.range 0 4
            in do
                it "renderMap" do
                    renderMap input `shouldStartWith` ".....OOOOO\nO"
        describe "bottom half column"
            let input =  Set.fromFoldable $ Tuple 9 <$> Array.range 5 9
            in do
                it "renderMap" do
                    renderMap input `shouldEndWith` """O
OOOOOOOOOO
OOOOOOOOO.
OOOOOOOOO.
OOOOOOOOO.
OOOOOOOOO.
OOOOOOOOO."""
