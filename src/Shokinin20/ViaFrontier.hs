{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}
module Shokinin20.ViaFrontier (ViaFrontier, Frontier) where

import Prelude

import GHC.Generics (Generic)
import Data.List (any)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import           Data.Maybe                     ( Maybe(..)
                                                , catMaybes
                                                )
import Control.Newtype (Newtype, unpack, pack)
import           Control.Monad                  ( guard )
import Data.Set (Set)
import Data.Set as Set

import Shokinin20.Rendering (renderLocation)
import Shokinin20.Internal (topX, topY, possibleNeighbours)
import Shokinin20.Types (Location, HasOfficePath(..), Office(..), Boolean)

type FrontierState = Map Location Frontier

newtype Frontier = Frontier (Maybe Location,Int)
  deriving (Generic, Eq, Show)

instance Newtype Frontier ((Maybe Location),Int)
{-- we always take the lowest score, since that was the one that got there first--}
instance Semigroup Frontier where
  (Frontier a@(l1,n1)) <> (Frontier b@(l2,n2)) = Frontier $ if n2 > n1 then a else b
instance Monoid Frontier where
  mempty = Frontier (Nothing,topY)

newtype ViaFrontier a = ViaFrontier (FrontierState,a)
instance Newtype (ViaFrontier a) (FrontierState,a)

instance HasOfficePath ViaFrontier where
  extractHasPath = snd.unpack
  calculateHasPath = frontierOffice

frontierOffice :: Office -> ViaFrontier Boolean
frontierOffice o@(Office officeStartingColumn officeSpaces) = head go
  where
        go = catMaybes $ zipWith loop (steps start) (tail $ steps start)
        steps :: ViaFrontier Int -> [ViaFrontier Int]
        steps v1 = v1 : steps (step v1)
        loop :: ViaFrontier Int -> ViaFrontier Int-> Maybe (ViaFrontier Boolean)
        loop v1 v2@(ViaFrontier (sol,depth)) = (ViaFrontier.(sol,)) <$> stop v1 v2

        start :: ViaFrontier Int
        start = frontierOne (officeStartingColumn, topY)
        step  :: ViaFrontier Int -> ViaFrontier Int
        step  = frontierSucc (flip Set.member officeSpaces)
        exitRow = (,0) <$> [0..topX]
        stop :: ViaFrontier Int -> ViaFrontier Int -> Maybe Boolean
        stop (ViaFrontier (state1,_)) (ViaFrontier (state2,_)) =
            if checkExit state2 then Just True
            else if stalled state1 state2 then Just False
            else Nothing
        checkExit st = any (flip Map.member st) exitRow
        stalled st1 st2 = st1==st2
{-# SCC frontierOffice #-}

frontierOne :: Location -> ViaFrontier Int
frontierOne origin = pack $ (newMap, 1)
  where
    newMap :: FrontierState
    newMap = Map.insert origin (Frontier ((Just origin), 1)) Map.empty

frontierSucc :: (Location -> Bool) -> ViaFrontier Int -> ViaFrontier Int
frontierSucc pred (ViaFrontier (state,depth)) = ViaFrontier (state', d')
  where
    state' = state <> frontier'

    frontier' :: FrontierState
    frontier' = Map.fromList $ do
      {-- we're going to work out where our frontier is, which is all the locations at the current depth.
          Then we're going to step in each direction from each frontier, with an increase in depth
          Then union all the new frontier spaces, relying on the monoid to pick the lowest depth.
          Our frontier will get smaller and smaller.  Callers will be able to check to see if the map has changed--}

      (loc,Frontier (seed,fDepth)) <- Map.toList state
      guard $ depth == fDepth
      mkAllKVs loc

    d' = depth+1
    mkAllKVs :: Location -> [(Location,Frontier)]
    mkAllKVs source = mkKV source <$> possibleFrontier source
    possibleFrontier :: Location -> [Location]
    possibleFrontier source = List.filter pred (possibleNeighbours source)
    mkKV :: Location -> Location -> (Location, Frontier)
    mkKV source key = (key,Frontier (Just source, d'))

renderFrontier :: Set Location -> FrontierState -> Location -> String
renderFrontier spaces state loc = case Map.lookup loc state of
    Just( Frontier( seed,depth) ) -> show depth
    Nothing -> renderLocation spaces loc