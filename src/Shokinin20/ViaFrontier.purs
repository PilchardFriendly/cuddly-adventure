module Shokinin20.ViaFrontier where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM2)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.List (List, any)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Unfoldable (class Unfoldable)
import Shokinin20 (Location, Office(..), renderLocation)
import Shokinin20.Internal (topX, topY, possibleNeighbours)
import Shokinin20.Types (class HasOfficePath)

type FrontierState = Map Location Frontier

newtype Frontier = Frontier ((Maybe Location) /\ Int)
derive instance genericFrontier :: Generic (Frontier) _
derive instance newtypeFrontier :: Newtype (Frontier) _
derive instance eqFrontier :: Eq (Frontier)
instance showGenericFrontier :: Show Frontier where
    show = genericShow
{-- we always take the lowest score, since that was the one that got there first--}
instance semigroupFrontier :: Semigroup Frontier where
  append (Frontier a@(l1 /\ n1)) (Frontier b@(l2 /\ n2)) = Frontier if n2 > n1 then a else b
instance monoidFrontier :: Monoid Frontier where
  mempty = Frontier (Nothing /\ top)

newtype ViaFrontier a = ViaFrontier (FrontierState /\ a)
derive instance newtypeViaFrontier :: Newtype (ViaFrontier a) _

instance hasOfficePathViaFrontier :: HasOfficePath ViaFrontier where
  extractHasPath = unwrap >>> snd
  calculateHasPath = frontierOffice

frontierOffice :: Office -> ViaFrontier Boolean
frontierOffice o@(Office {officeStartingColumn, officeSpaces}) = unwrap $ go start
  where 
        go v1 = tailRecM2 loop v1 (step v1)
        loop :: ViaFrontier Int -> ViaFrontier Int-> Identity (Step { a :: ViaFrontier Int, b :: ViaFrontier Int} (ViaFrontier Boolean))
        loop v1 v2@(ViaFrontier (sol/\depth)) = case (stop v1 v2) of
            Just b -> pure $ Done (ViaFrontier (sol/\b))
            Nothing -> pure $ Loop { a : v2, b : step v2 }
        
        start :: ViaFrontier Int
        start = frontierOne (officeStartingColumn /\ topY)
        step  :: ViaFrontier Int -> ViaFrontier Int
        step  = frontierSucc (flip Set.member officeSpaces)
        exitRow = (\i -> i/\0) <$> Array.range 0 topX
        stop :: ViaFrontier Int -> ViaFrontier Int -> Maybe Boolean
        stop (ViaFrontier (state1/\_)) (ViaFrontier (state2/\_)) = 
            if checkExit state2 then Just true
            else if stalled state1 state2 then Just false
            else Nothing
        checkExit st = any (flip Map.member st) exitRow
        stalled st1 st2 = st1==st2

frontierOne :: Location -> ViaFrontier Int
frontierOne origin = wrap $ newMap /\ 1
  where 
    newMap :: FrontierState
    newMap = Map.insert origin (Frontier ((Just origin) /\ 1)) Map.empty

frontierSucc :: (Location -> Boolean) -> ViaFrontier Int -> ViaFrontier Int
frontierSucc pred (ViaFrontier (state/\depth)) = ViaFrontier (state' /\ d')
  where 
    state' = state <> frontier'

    frontier' :: FrontierState 
    frontier' = Map.fromFoldable $ do
      {-- we're going to work out where our frontier is, which is all the locations at the current depth.
          Then we're going to step in each direction from each frontier, with an increase in depth
          Then union all the new frontier spaces, relying on the monoid to pick the lowest depth.
          Our frontier will get smaller and smaller.  Callers will be able to check to see if the map has changed--}

      loc/\Frontier (seed/\fDepth) <- Map.toUnfoldable state
      guard $ depth == fDepth
      next <- mkAllKVs loc
      pure next
    d' = depth+1
    mkAllKVs :: Location -> List (Location /\ Frontier)
    mkAllKVs source = mkKV source <$> possibleFrontier source
    possibleFrontier :: forall f. Unfoldable f => Location -> f Location
    possibleFrontier source =  List.toUnfoldable $ List.filter pred (possibleNeighbours source)
    mkKV :: Location -> Location -> Location /\ Frontier
    mkKV source key = key /\ (Frontier $ (Just source) /\ d')
  
renderFrontier :: Set Location -> FrontierState -> Location -> String
renderFrontier spaces state loc = case (Map.lookup loc state) of
    Just( Frontier( seed/\depth) ) -> show depth
    Nothing -> renderLocation spaces loc