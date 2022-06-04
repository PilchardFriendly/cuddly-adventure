{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Shokinin20.ViaGraph (officeHasPath, officeGraph, ViaGraph, OfficeGraph, HasOfficePath(..)) where

import Prelude
import Data.Graph (Graph)
import Data.Graph as Graph
-- import Data.List (List, any, foldr)
import Data.List as List
import qualified Data.Foldable  as Foldable
import Control.Newtype (Newtype, unpack)
import Data.Set (Set)
import Data.Set as Set
import Shokinin20.Internal (possibleNeighbours, topX, topY)
import Shokinin20.Types (HasOfficePath(..), Location, Office(..))

newtype ViaGraph a
  = ViaGraph a
instance Newtype (ViaGraph a) a

instance HasOfficePath ViaGraph where
  extractHasPath = unpack
  calculateHasPath = ViaGraph . officeHasPath

{-- officeHasPath: True if there is a path from (n,topY)-> (m, 0) where n is the starting position--}
officeHasPath :: Office -> Bool
{-# SCC officeHasPath #-}

{-- Using connectedComponents is brute force - since it will find all possible paths, effectively.
    It gets cheaper the more full of obstacles the office becomes
    but it's a useful library to get started with--}
officeHasPath o@(Office officeStartingColumn  officeSpaces) =
    any containsStartAndEnd $ subGraphs mkGraph
  where
    mkGraph :: OfficeGraph
    mkGraph = officeGraph' officeSpaces
    subGraphs :: OfficeGraph -> [[Location]]
    subGraphs (g,lookup,_) =  fmap (toLocation.lookup) <$> sccVerticea g
    toLocation :: (Location,Location,[Location]) -> Location
    toLocation (l,_,_) = l
    sccVerticea :: Graph -> [[Vertex]]
    sccVerticea g = Foldable.toList . Foldable.toList <$> Graph.scc g
    containsStartAndEnd :: [Location] -> Bool
    containsStartAndEnd locations = any isExit locations && any isStart locations

    isExit :: Location -> Bool
    isExit t = 0 == snd t

    isStart :: Location -> Bool
    isStart t = t == (officeStartingColumn,topY)

type FoldR a b
  = a -> b -> b

type Builder a
  = a -> a

type OfficeGraph = (Graph, Vertex -> (Location, Location, [Location]), Location -> Maybe Vertex)

{-- Turns a set of points into a di-graph based on cartesian adjacency (not diagonals)--}
officeGraph :: Set Location -> OfficeGraph
officeGraph = officeGraph'

officeGraph' :: Set Location -> (Graph, Vertex -> (Location, Location, [Location ]), Location -> Maybe Vertex)
officeGraph' spaces = Graph.graphFromEdges edges
  where
    edges :: [(Location, Location, [Location])]
    edges = edge <$> toList spaces

    edge :: Location -> (Location, Location, [Location])
    edge l = (l, l, List.filter isSpace (possibleNeighbours l))

    locationVertex :: Location -> Vertex
    locationVertex (x,y) = (y*topX) + x

    isSpace :: Location -> Bool
    isSpace = flip Set.member spaces