{-# LANGUAGE RankNTypes #-}
module Shokinin20.Shokinin20
  ( experiment
  , harness
  , genStartingColumn
  , office
  ) where

import Prelude hiding (log)

import Data.Ratio ((%))

import Hedgehog.Internal.Gen (MonadGen, evalGen, Gen, fromGenT, GenT(..))
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
import qualified System.Random.SplitMix as SplitMix
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


genRatio :: (MonadGen m)  => m Double
genRatio = fromGenT . GenT $ \_ (Seed v0 _) -> pure (f (SplitMix.mkSMGen v0))
    where f seed = fst $ SplitMix.nextDouble seed

genProbabilities :: (MonadGen m) => m [Probability]
genProbabilities = fromGenT . GenT $ \_ (Seed v0 gamma) -> pure (fix ratios (SplitMix.mkSMGen v0))
  where
    ratios :: (SplitMix.SMGen -> [Probability]) -> SplitMix.SMGen -> [Probability]
    ratios recur seed = let next = SplitMix.nextDouble seed
                        in pack (fst next) : recur (snd next)

genBool' :: MonadGen m => Probability -> m Bool
genBool' (Probability p) = do
  sample <- genRatio
  pure (sample < p)

genMaybe' :: MonadGen m => Probability -> m a -> m (Maybe a)
genMaybe' (Probability p) ma = do
  sample <- genRatio
  if sample < p then Just <$> ma else pure Nothing


genObstacles :: forall m. MonadGen m => Probability -> [Location] -> m (Set Location)
genObstacles (Probability p) xs =  flatten <$> do
  samples <- genProbabilities
  pure $ zipWith sample samples xs
  where
    flatten :: [Maybe Location] -> Set Location
    flatten = Set.fromList . catMaybes
    sample :: Probability -> Location -> Maybe Location
    sample (Probability b) a = if b < p then Just a else Nothing

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
