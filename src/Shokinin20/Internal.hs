module Shokinin20.Internal (topX, topY, allLocations, possibleNeighbours) where

import Prelude

import Control.Arrow ((***), first, second)
import Data.Traversable (sequence)
import Shokinin20.Types (Location)

topY :: Int
topY = 9
topX :: Int
topX = 9

allLocations :: [Location]
allLocations = do
    x <- [0..topX]
    y <- [0..topY]
    pure (x,y)

{-- looks up, down, left, and right--}
possibleNeighbours :: Location -> [Location]
possibleNeighbours = sequence offsets
  where
  offsets = [ first inc,first dec, second inc, second dec ]

  inc :: Int -> Int
  inc = (+) 1

  dec :: Int -> Int
  dec = flip (-) 1