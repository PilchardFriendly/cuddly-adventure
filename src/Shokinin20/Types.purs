module Shokinin20.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, wrap)
import Data.Set (Set)
import Data.Tuple (Tuple)
  
type Location = Tuple Int Int

class HasOfficePath algo where
  extractHasPath :: algo Boolean -> Boolean
  calculateHasPath :: Office -> algo Boolean


data Office = Office {
    officeStartingColumn :: Int
    ,officeSpaces :: Set Location  -- keyed by tuple e.g. (1,5)  
}


-- Probabilty is a float between 0 and 1

newtype Probability = Probability Number
instance boundedProbability :: Bounded Probability where
    top = wrap 1.0
    bottom = wrap 0.0
derive instance genericProbability :: Generic Probability _
derive instance newtypeProbability :: Newtype Probability _
derive instance eqProbability :: Eq Probability
derive instance ordProbability :: Ord Probability
instance showProbability :: Show Probability where
  show = genericShow

probInvert :: Probability -> Probability
probInvert = over Probability (\p -> 1.0 - p)