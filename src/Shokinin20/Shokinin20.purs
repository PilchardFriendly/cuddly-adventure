module Shokinin20
  ( module Shokinin20.Types
  , experiment
  , harness
  , officeHasPath
  , genStartingColumn
  , office
  , officeGraph
  , possibleNeighbours
  , module Shokinin20.Rendering
  ) where

import Prelude

import Control.Biapply (biapply)
import Control.Monad.Gen (class MonadGen, chooseInt, resize, unfoldable)
import Control.Monad.Gen.Common (genMaybe')
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (any, foldr)
import Data.Foldable (class Foldable, length, traverse_)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.Int (toNumber)
import Data.List (List)
import Data.List (catMaybes, concatMap, filter, fromFoldable) as List
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set (fromFoldable, insert, member, toUnfoldable) as Set
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Random.LCG (Seed)
import Shokinin20.Internal (allLocations, topX, topY)
import Shokinin20.Rendering (locationParser, mapParser, parseMap, renderLine', renderLocation, renderMap, renderMap')
import Shokinin20.Types (Location, Office(..), Probability(..), probInvert)
import Teletype (Teletype, log)
import Test.QuickCheck.Gen (Gen, evalGen)

office :: Set Location -> Int -> Office
office spaces startingColumn =
  Office
    { officeStartingColumn: startingColumn
    , officeSpaces: Set.insert (Tuple startingColumn topY) spaces
    }

{-- officeHasPath: True if there is a path from (n,topY)-> (m, 0) where n is the starting position--}
officeHasPath :: Office -> Boolean
{-- Using connectedComponents is brute force - since it will find all possible paths, effectively.
    It gets cheaper the more full of obstacles the office becomes
    but it's a useful library to get started with--}
officeHasPath o@(Office { officeStartingColumn, officeSpaces }) = any containsStartAndEnd $ Graph.vertices <$> (Graph.connectedComponents $ officeGraph officeSpaces)
  where
  containsStartAndEnd :: List Location -> Boolean
  containsStartAndEnd locations = any isExit locations && any isStart locations

  isExit :: Location -> Boolean
  isExit t = 0 == snd t

  isStart :: Location -> Boolean
  isStart t = t == Tuple officeStartingColumn topY

{-- looks up, down, left, and right--}
possibleNeighbours :: Location -> List Location
possibleNeighbours l = sequence (List.fromFoldable offsets) l
  where
  offsets = biapply <$> [ Tuple inc identity, Tuple dec identity, Tuple identity inc, Tuple identity dec ]

  inc :: Int -> Int
  inc = (+) 1

  dec :: Int -> Int
  dec = flip (-) 1

type FoldR a b
  = a -> b -> b

type Builder a
  = a -> a

{-- Turns a set of points into a di-graph based on cartesian adjacency (not diagonals)--}
officeGraph :: Set Location -> Graph Location Int
officeGraph spaces = buildAllEdges $ buildAllVertices $ Graph.empty
  where
  allVertices :: List Location
  allVertices = Set.toUnfoldable spaces

  isSpace :: Location -> Boolean
  isSpace = flip Set.member spaces

  {-- Only neighbours that are in the set of spaces count --}
  neighbouringEdges :: Location -> List (Tuple Location Location)
  neighbouringEdges l = Tuple l <$> (List.filter isSpace $ possibleNeighbours l)

  allEdges :: List (Tuple Location Location)
  allEdges = List.concatMap neighbouringEdges allVertices

  buildAllEdges :: Builder (Graph Location Int)
  buildAllEdges g = foldr edge g allEdges

  edge :: FoldR (Tuple Location Location) (Graph Location Int)
  edge t g = (uncurry Graph.insertEdge) t 1 g

  buildAllVertices :: Builder (Graph Location Int)
  buildAllVertices g = foldr Graph.insertVertex g allVertices

-- genMaybeP :: forall m a. MonadGen m => Probability -> a -> m (Maybe a)
-- genMaybeP p a = genMaybe' (unwrap p) (pure a)
genObstacles :: forall m. MonadGen m => Probability -> List Location -> m (Set Location)
genObstacles prob xs = Set.fromFoldable <<< List.catMaybes <$> traverse sample xs
  where
  sample :: Location -> m (Maybe Location)
  sample a = genMaybe' (unwrap prob) (pure a)

genStartingColumn :: forall m. MonadGen m => m Int
genStartingColumn = chooseInt 0 topX

officeGen :: forall m. MonadGen m => Probability -> m Office
officeGen bias = do
  obstacles <- genObstacles (probInvert bias) allLocations
  start <- genStartingColumn
  pure $ office obstacles start

{-- The main event.  The probability is the _chance of an obstacle_ --}
experiment :: forall m. MonadRec m => MonadGen m => Probability -> Int -> m Probability
experiment bias trialCount = do
  trials :: List Office <- resize (const trialCount) $ unfoldable $ officeGen bias
  pure $ (calculate trials)
  where
  calculate :: List Office -> Probability
  calculate = wrap <<< (flip div $ toNumber trialCount) <<< toNumber <<< length <<< (List.filter officeHasPath)

harness :: forall f. Foldable f => Seed -> f Probability -> Int -> (Probability -> Int -> Gen Probability) -> Teletype Unit
harness seed biases n f = do
  log $ "Number of samples for each p: " <> (show n)
  traverse_  (go >>> log) biases
  where
  go :: Probability -> String
  go bias = joinWith " " $ show <<< unwrap <$> sequence [ identity, goGen ] bias
  -- Reminder, sequence (in this case) is like juxt in clojure
    -- e.g. [f1 f2] v ---> [f1 v, f2 v]
    -- in list of function `[i -> o]` , 
    --      t ~ List, 
    --      m ~ i->
    --      a ~ o

    -- If you look at the Applicative instance for Function, it might make sense.  This is very common in haskellish languages
    -- and also in clojure.  Hence, also look at clojure/juxt

  goGen :: Probability -> Probability
  goGen bias = evalGen (f bias n) {newSeed: seed, size: 1}
