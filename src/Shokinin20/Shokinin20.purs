module Shokinin20
  ( module Shokinin20.Types
  , experiment
  , harness
  , genStartingColumn
  , office
  , module Shokinin20.ViaGraph
  , module Shokinin20.Rendering
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt, resize, unfoldable)
import Control.Monad.Gen.Common (genMaybe')
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable, length, traverse_)
import Data.Int (toNumber)
import Data.List (List)
import Data.List (catMaybes, filter) as List
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set (fromFoldable, insert) as Set
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Random.LCG (Seed)
import Shokinin20.Internal (allLocations, topX, topY)
import Shokinin20.Rendering (locationParser, mapParser, parseMap, renderLine', renderLocation, renderMap, renderMap')
import Shokinin20.Types (class HasOfficePath, Location, Office(..), Probability(..), calculateHasPath, extractHasPath, probInvert)
import Shokinin20.ViaGraph (officeGraph, officeHasPath)
import Teletype (Teletype, log)
import Test.QuickCheck.Gen (Gen, evalGen)
import Text.Format (format, precision)

office :: Set Location -> Int -> Office
office spaces startingColumn =
  Office
    { officeStartingColumn: startingColumn
    , officeSpaces: Set.insert (Tuple startingColumn topY) spaces
    }

-- genMaybeP :: forall m a. MonadGen m => Probability -> a -> m (Maybe a)
-- genMaybeP p a = genMaybe' (unwrap p) (pure a)
genObstacles :: forall m. MonadGen m => Probability -> List Location -> m (Set Location)
genObstacles prob xs =  flatten <$> traverse sample xs
  where
  flatten = Set.fromFoldable <<< List.catMaybes
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
experiment :: forall m. MonadRec m => MonadGen m => Probability -> Int -> (Office -> Boolean) -> m (Probability)
experiment bias  trialCount solver= do
  trials :: List Office <- resize (const trialCount) 
    $ unfoldable 
    $ officeGen bias
  pure $ (calculate trials)
  where
  calculate :: List Office -> Probability
  calculate = wrap 
    <<< (flip div $ toNumber trialCount) 
    <<< toNumber 
    <<< length 
    <<< (List.filter solver)


harness :: forall f. Foldable f => Seed -> f Probability -> Int -> (Office->Boolean) -> (Probability -> Int -> (Office->Boolean) -> Gen Probability) -> Teletype Unit
harness seed biases n sol f = do
  log $ "Number of samples for each p: " <> (show n)
  traverse_  (go >>> log) biases
  where
  go :: Probability -> String
  go bias = joinWith " " $ sequence [ show <<< unwrap <<< identity 
                                    , format (precision 3) <<< unwrap <<< goGen ] bias
  -- Reminder, sequence (in this case) is like juxt in clojure
    -- e.g. [f1 f2] v ---> [f1 v, f2 v]
    -- in list of function `[i -> o]` , 
    --      t ~ List, 
    --      m ~ i->
    --      a ~ o

    -- If you look at the Applicative instance for Function, it might make sense.  This is very common in haskellish languages
    -- and also in clojure.  Hence, also look at clojure/juxt

  goGen :: Probability -> Probability
  goGen bias = evalGen (f bias n sol) {newSeed: seed, size: 1}
