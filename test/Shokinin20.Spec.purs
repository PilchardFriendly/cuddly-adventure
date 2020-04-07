module Shokinin20.Spec(spec) where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseFloat)
import Data.Array (range, replicate) as Array
import Data.List (List, catMaybes, range)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (empty, fromFoldable, member) as Set
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldEndWith, shouldStartWith)

type Location = Tuple Int Int

data Puzzle = Puzzle {
    puzzleStartingColumn :: Int
    ,obstacles :: Set Location  -- keyed by tuple e.g. (1,5)  
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
genObstacles prob xs = Set.fromFoldable <<< catMaybes <$> traverse sample xs
    where
        sample :: Location -> m (Maybe Location)
        sample = genMaybeP prob

topY :: Int
topY = 9
topX :: Int
topX = 9

allLocations :: List Location
allLocations = do
    x <- range 0 topX
    y <- range 0 topY
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

spec :: Spec Unit
spec = describe "Shokinin 20" 
    let emptyMap = Set.fromFoldable allLocations
        fullMap = Set.empty in do

    -- describe "pathfinding" do
    --     it "empty map"
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
