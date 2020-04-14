module Shokinin20.Internal where

import Prelude

import Control.Biapply (biapply)
import Data.List (List)
import Data.List (fromFoldable, range) as List
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Shokinin20.Types (Location)

topY :: Int
topY = 9
topX :: Int
topX = 9


allLocations :: List Location
allLocations = do
    x <- List.range 0 topX
    y <- List.range 0 topY
    pure $ Tuple x y

{-- looks up, down, left, and right--}
possibleNeighbours :: Location -> List Location
possibleNeighbours l = sequence (List.fromFoldable offsets) l
  where
  offsets = biapply <$> [ Tuple inc identity, Tuple dec identity, Tuple identity inc, Tuple identity dec ]

  inc :: Int -> Int
  inc = (+) 1

  dec :: Int -> Int
  dec = flip (-) 1    