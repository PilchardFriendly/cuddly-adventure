module Shokinin20.ViaGraph (officeHasPath, officeGraph, ViaGraph) where

import Prelude
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List, any, foldr)
import Data.List as List
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd, uncurry)
import Shokinin20.Internal (possibleNeighbours, topY)
import Shokinin20.Types (class HasOfficePath, Location, Office(..))

newtype ViaGraph a
  = ViaGraph a

derive instance newtypeViaGraph :: Newtype (ViaGraph a) _

instance hasOfficePathViaGraph :: HasOfficePath ViaGraph where
  extractHasPath = unwrap
  calculateHasPath = ViaGraph <<< officeHasPath

{-- officeHasPath: True if there is a path from (n,topY)-> (m, 0) where n is the starting position--}
officeHasPath :: Office -> Boolean
{-- Using connectedComponents is brute force - since it will find all possible paths, effectively.
    It gets cheaper the more full of obstacles the office becomes
    but it's a useful library to get started with--}
officeHasPath o@(Office { officeStartingColumn, officeSpaces }) = any containsStartAndEnd $ Graph.vertices <$> (Graph.connectedComponents $ officeGraph officeSpaces)
  where
  containsStartAndEnd :: List Location -> Boolean
  containsStartAndEnd locations = any isExit locations && any isStart locations

  isExit :: Location -> Boolean
  isExit t = 0 == snd t

  isStart :: Location -> Boolean
  isStart t = t == Tuple officeStartingColumn topY

type FoldR a b
  = a -> b -> b

type Builder a
  = a -> a

{-- Turns a set of points into a di-graph based on cartesian adjacency (not diagonals)--}
officeGraph :: Set Location -> Graph Location Int
officeGraph spaces = buildAllEdges $ buildAllVertices $ Graph.empty
  where
  allVertices :: List Location
  allVertices = Set.toUnfoldable spaces

  isSpace :: Location -> Boolean
  isSpace = flip Set.member spaces

  {-- Only neighbours that are in the set of spaces count --}
  neighbouringEdges :: Location -> List (Tuple Location Location)
  neighbouringEdges l = Tuple l <$> (List.filter isSpace $ possibleNeighbours l)

  allEdges :: List (Tuple Location Location)
  allEdges = List.concatMap neighbouringEdges allVertices

  buildAllEdges :: Builder (Graph Location Int)
  buildAllEdges g = foldr edge g allEdges

  edge :: FoldR (Tuple Location Location) (Graph Location Int)
  edge t g = (uncurry Graph.insertEdge) t 1 g

  buildAllVertices :: Builder (Graph Location Int)
  buildAllVertices g = foldr Graph.insertVertex g allVertices
