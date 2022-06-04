module Main(main) where

import Prelude

import Control.Newtype (pack)
import Text.Read (Read(..), readsPrec)
import qualified Options.Applicative as Options
import Options.Applicative (execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, strOption, value, (<**>))
import Hedgehog.Internal.Seed (Seed(..))
import Shokinin20.Shokinin20 (experiment, harness)
import Data.Ratio ((%))
import System.Random
import Shokinin20.Types (HasOfficePath(..), Office, Probability)
import Shokinin20.ViaFrontier (ViaFrontier)
import Shokinin20.ViaGraph (ViaGraph)
import Teletype (Teletype, teletypeToIO)
import Polysemy

data Strategy = StrategyGraph | StrategyFrontier | StrategyShortMap
instance Read Strategy where
  readsPrec _ s = case read s of
    Just "graph" -> [(StrategyGraph,"")]
    Just "frontier" -> [(StrategyFrontier,"")]
    Just "shortmap" -> [(StrategyShortMap, "")]
    _ -> []

mkStrategy :: String -> Maybe Strategy
mkStrategy "graph" = Just StrategyGraph
mkStrategy  "frontier" = Just StrategyFrontier
mkStrategy "shortmap" = Just StrategyShortMap
mkStrategy _ = Nothing

newtype Program = Program (Maybe Strategy)

newtype ViaShortMap a = ViaShortMap a
instance HasOfficePath ViaShortMap where
  extractHasPath _ = False
  calculateHasPath _ = ViaShortMap False

programOptions :: Options.Parser Program
programOptions = Program  <$> (mkStrategy <$> strOption (
        long "strategy"
        <> metavar "STRATEGY"
        <> value "frontier"
        <> help "How to solve the problem"))

program :: (Member Teletype r) => (Office -> Bool) -> Seed -> Sem r ()
program solver seed = harness seed biases samples solver experiment
  where
    biases :: [Probability]
    biases = mkBias <$> [10,9..0]
    mkBias :: Int -> Probability
    mkBias n = pack $ fromRational $ toInteger n % 10
    samples :: Int
    samples = 1000


{--- maybe you're just joining tonights show. --}
main :: IO ()
main = do
  seed <- flip Seed 1 <$> randomIO
  opts <- options
  runM $ teletypeToIO (program (strategy opts) seed)
  where
    options = execParser $ info (programOptions <**> helper) ( fullDesc
      <> progDesc "Shokinin 20"
      <> header "Get to the food truck!")
    strategy :: Program -> (Office -> Bool)
    strategy (Program s) = case s of
      Just StrategyGraph -> extractHasPath . viaGraph
      Just StrategyFrontier -> extractHasPath . viaFrontier
      Just StrategyShortMap -> extractHasPath . viaShortMap
      Nothing -> extractHasPath . viaGraph
    viaGraph :: Office -> ViaGraph Bool
    viaGraph = calculateHasPath
    viaFrontier :: Office -> ViaFrontier Bool
    viaFrontier = calculateHasPath
    viaShortMap :: Office -> ViaShortMap Bool
    viaShortMap = calculateHasPath


