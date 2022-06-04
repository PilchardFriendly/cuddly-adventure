{-# LANGUAGE
    DerivingStrategies,
    DerivingVia,
    DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Shokinin20.Types
  ( HasOfficePath(..)
  , Number
  , Boolean
  , Location
  , Office(..)
  , Probability(..)
  , probInvert
  )
where

import           Prelude

import           GHC.Generics                   ( Generic )
import           Control.Newtype                ( over
                                                , Newtype
                                                )
import           Data.Set                       ( Set )

type Number = Double
type Boolean = Bool
type Location = (Int, Int)

class HasOfficePath algo where
  extractHasPath :: algo Bool -> Bool
  calculateHasPath :: Office -> algo Bool


data Office = Office {
    officeStartingColumn :: Int
    ,officeSpaces :: Set Location  -- keyed by tuple e.g. (1,5)  
}


-- Probabilty is a float between 0 and 1

newtype Probability = Probability Number
  deriving Generic
  deriving Eq
  deriving Ord
  deriving Show
instance Newtype Probability Double
-- instance Bounded Probability where
--     top = wrap 1.0
--     bottom = wrap 0.0

probInvert :: Probability -> Probability
probInvert = over Probability (1.0 -)
