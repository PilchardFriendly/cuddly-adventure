module Shokinin20.Internal where

import Prelude
import Shokinin20.Types (Location)
import Data.List (List)
import Data.List (range) as List
import Data.Tuple (Tuple(..))

topY :: Int
topY = 9
topX :: Int
topX = 9


allLocations :: List Location
allLocations = do
    x <- List.range 0 topX
    y <- List.range 0 topY
    pure $ Tuple x y