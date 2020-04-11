module Main(main) where

import Prelude
import Data.Int (toNumber)
import Data.Newtype (wrap)
import Data.Unfoldable (range) as Unfoldable
import Effect (Effect)
import Random.LCG (Seed, randomSeed)
import Shokinin20 (Probability, experiment, harness)
import Teletype (Teletype, runTeletype)

program :: Seed -> Teletype Unit
program seed = harness seed biases samples experiment
  where 
    biases :: Array Probability
    biases = mkBias <$> (Unfoldable.range 10 0)
    mkBias :: Int -> Probability
    mkBias = toNumber >>> (flip div 10.0) >>> wrap 
    samples :: Int
    samples = 1000

{--- maybe you're just joining tonights show. --}
main :: Effect Unit
main = randomSeed >>= program >>> runTeletype
