module Main(main) where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String.Read (class Read, read)
import Data.Unfoldable (range) as Unfoldable
import Effect (Effect)
import Options.Applicative (Parser) as Options
import Options.Applicative (execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, strOption, value, (<**>))
import Random.LCG (Seed, randomSeed)
import Shokinin20 (Office, Probability, calculateHasPath, experiment, extractHasPath, harness)
import Shokinin20.Frontier (ViaFrontier)
import Shokinin20.ViaGraph (ViaGraph)
import Teletype (Teletype, runTeletype)

data Strategy = StrategyGraph | StrategyFrontier
instance readStrategy :: Read Strategy where
  read s = case read s of
    Just "graph" -> Just StrategyGraph
    Just "frontier" -> Just StrategyFrontier
    _ -> Nothing

data Program = Program (Maybe Strategy)

programOptions :: Options.Parser Program
programOptions = Program  <$> (read <$> (strOption (long "strategy" <> metavar "STRATEGY" <> value "frontier" <> help "How to solve the problem")))

program :: (Office -> Boolean) -> Seed -> Teletype Unit
program solver seed = harness seed biases samples solver experiment
  where 
    biases :: Array Probability
    biases = mkBias <$> (Unfoldable.range 10 0)
    mkBias :: Int -> Probability
    mkBias = toNumber >>> (flip div 10.0) >>> wrap 
    samples :: Int
    samples = 1000


{--- maybe you're just joining tonights show. --}
main :: Effect Unit
main = do
  seed <- randomSeed
  opts <- options
  logs <- runTeletype (program (strategy opts) seed)
  pure logs
  where
    options = execParser $ info (programOptions <**> helper) ( fullDesc
     <> progDesc "Shokinin 20"
     <> header "Get to the food truck!")
    strategy :: Program -> (Office -> Boolean)
    strategy (Program s) = case s of 
      Just (StrategyGraph) -> (extractHasPath <<<viaGraph)
      Just (StrategyFrontier) -> (extractHasPath <<< viaFrontier)
      Nothing -> (extractHasPath <<< viaGraph)
    viaGraph :: Office -> ViaGraph Boolean
    viaGraph = calculateHasPath
    viaFrontier :: Office -> ViaFrontier Boolean
    viaFrontier = calculateHasPath


