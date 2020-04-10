module Main where

import Prelude

import Data.Int (toNumber)
import Data.Newtype (wrap)
import Data.Unfoldable (range) as Unfoldable
import Effect (Effect)
import Random.LCG (Seed, randomSeed)
import Shokinin20 (Probability, experiment, harness)
import Teletype (Teletype, runTeletype)

program :: Seed -> Teletype Unit
program seed = harness seed biases 100 (\bias samples -> experiment bias samples)
  where 
    biases :: Array Probability
    biases = mkBias <$> (Unfoldable.range 0 10)
    mkBias :: Int -> Probability
    mkBias = toNumber >>> (flip div 10.0) >>> wrap 

main :: Effect Unit
main = do
  seed <- randomSeed
  runTeletype $ program seed
