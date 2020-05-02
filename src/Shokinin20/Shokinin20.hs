{-# LANGUAGE RankNTypes #-}
module Shokinin20.Shokinin20
  ( experiment
  , harness
  , genStartingColumn
  , office
  ) where

import Prelude hiding (log)

import Data.Ratio ((%))

import Hedgehog.Internal.Gen (MonadGen, evalGen, Gen)
import Hedgehog.Internal.Seed (Seed(..))
import Hedgehog.Internal.Tree (treeValue)
import Hedgehog.Range (Size(..))
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Data.Maybe (catMaybes)
import Formatting (format)
import qualified Formatting.Formatters as Format
import Control.Newtype (unpack, pack)
import Control.Monad.Identity
import Polysemy
import Text.Printf (printf)

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Shokinin20.Internal (allLocations, topX, topY)
-- import Shokinin20.Rendering (locationParser, mapParser, parseMap, renderLine', renderLocation, renderMap, renderMap')
import Shokinin20.Types (HasOfficePath, Location, Office(..), Probability(..), calculateHasPath, extractHasPath, probInvert)
-- import Shokinin20.ViaGraph (officeGraph, officeHasPath)
import Teletype (Teletype(..), log)


type Unit = ()

office :: Set Location -> Int -> Office
office spaces startingColumn = Office startingColumn (Set.insert (startingColumn,topY) spaces)

genMaybe' :: MonadGen m => Probability -> m a -> m (Maybe a)
genMaybe' (Probability p) ma = do
  sample <- Gen.double  (Range.linearFrac 0 1)
  if (sample < p) then (Just <$> ma) else pure Nothing

genObstacles :: forall m. MonadGen m => Probability -> [Location] -> m (Set Location)
genObstacles prob xs =  flatten <$> traverse sample xs
  where
  flatten = Set.fromList . catMaybes
  sample :: Location -> m (Maybe Location)
  sample a = genMaybe' prob (pure a)

genStartingColumn :: forall m. MonadGen m => m Int
genStartingColumn = Gen.int (Range.linear 0 topX)

officeGen :: forall m. MonadGen m => Probability -> m Office
officeGen bias = do
  obstacles <- genObstacles (probInvert bias) allLocations
  office obstacles <$> genStartingColumn

{-- The main event.  The probability is the _chance of an obstacle_ --}
experiment :: forall m. MonadGen m => Probability -> Int -> (Office -> Bool) -> m Probability
experiment bias  trialCount solver= do
  trials <- Gen.list (Range.singleton trialCount) $ officeGen bias
  pure (calculate trials)
  where
    chance :: Integer -> Probability
    chance successes = pack . fromRational $ successes % toInteger trialCount
    calculate :: [Office] -> Probability
    calculate = chance
      . toInteger
      . length
      . filter solver

harness :: Member Teletype r
        => Foldable f
        => Seed
        -> f Probability
        -> Int
        -> (Office->Bool)
        -> (Probability -> Int -> (Office->Bool) -> Gen Probability)
        -> Sem r ()
harness seed biases n sol f = do
  log $ "Number of samples for each p: " <> show n
  mapM_ (log . go) biases
  where
  go :: Probability -> String
  go bias = printf "%.1f %.3f" (unpack bias) (unpack $ goGen bias)
  -- Reminder, sequence (in this case) is like juxt in clojure
    -- e.g. [f1 f2] v ---> [f1 v, f2 v]
    -- in list of function `[i -> o]` , 
    --      t ~ List, 
    --      m ~ i->
    --      a ~ o

    -- If you look at the Applicative instance for Function, it might make sense.  This is very common in haskellish languages
    -- and also in clojure.  Hence, also look at clojure/juxt

  goGen :: Probability -> Probability
  goGen bias = case evalGen 1000 seed (f bias n sol) of
    (Just t) -> treeValue t
    Nothing -> error "No generated Value"
