module Shokinin20.LogicT where

import Prelude

import Control.Monad.Logic.Class (class MonadLogic)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, findIndex)
import Data.Array.NonEmpty as NonEmptyArray
import Data.List as List
import Data.Maybe (Maybe(..), isNothing, maybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable)
import Shokinin20 (Location, Office(..))
import Shokinin20.Internal (possibleNeighbours, topY)
import Shokinin20.Types (class HasOfficePath)


type SearchState = NonEmptyArray Location

newtype ViaBacktracking a = ViaBacktracking (Tuple (Maybe SearchState) a)
derive instance newtypeViaBacktracking :: Newtype (ViaBacktracking a) _

instance hasOfficePathViaBacktracking :: HasOfficePath ViaBacktracking where
  extractHasPath = unwrap >>> snd
  calculateHasPath = findPaths >>> bestPath

findPaths :: forall m. Unfoldable m => MonadLogic m => Office -> m SearchState
findPaths o@(Office {officeStartingColumn, officeSpaces})= go
  where
    go = findPaths' newPath officeSpaces
    newPath = NonEmptyArray.singleton (officeStartingColumn /\ topY)

findPaths' :: forall m. Unfoldable m => MonadLogic m => SearchState -> Set Location -> m SearchState
findPaths' path theMap | (snd $ NonEmptyArray.head $ path) == 0 = pure path
findPaths' path theMap = do
  next <- nextPath
  findPaths' next theMap
  
  where 
    nextPath :: m SearchState
    nextPath = do
      neighbour <- List.toUnfoldable $ possibleNeighbours cursor
      guard ((isNothing $ findIndex (eq neighbour) path ) && (Set.member neighbour theMap))
      pure $ NonEmptyArray.cons neighbour path
    cursor = (NonEmptyArray.head path)

bestPath :: Array SearchState -> ViaBacktracking Boolean
bestPath ss = ViaBacktracking $ maybe' f t (Array.head ss)
  where
    t a = (Just a) /\ true
    f _ = Nothing /\ false
