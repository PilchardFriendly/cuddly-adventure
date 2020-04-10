module Shokinin20.Rendering where

import Prelude

import Control.Alt ((<|>))
import Data.Array (range, many) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List)
import Data.Set (Set)
import Data.Set (empty, insert, member) as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Shokinin20.Types (Location)
import Shokinin20.Internal (topX, topY)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1) as P
import Text.Parsing.Parser.String (char) as P


renderLocation ::  Set Location -> Location -> String
renderLocation locations location = if (Set.member location locations) then "." else "O"
    

renderLine' :: (Location -> String)  -> Int -> Array Int -> String
renderLine' f y xs = joinWith "" do
    x <- xs
    pure $ f (Tuple x y)    

renderMap :: Set Location -> String
renderMap locations = renderMap' (renderLocation locations) locations

renderMap' ::  (Location -> String) -> Set Location -> String
renderMap' f locations = joinWith "\n" do
    y <- Array.range 0 topY
    pure $ renderLine' f y (Array.range 0 topX)

parseMap :: String -> Either String (Set Location)
parseMap s = lmap (const "Argh") $ runParser s $ mapParser locationParser

locationParser :: Parser String Boolean
locationParser = ((const true) <$> P.char '.') <|> ((const false <$> P.char 'O'))

mapParser :: Parser String Boolean -> Parser String (Set Location)
mapParser locParser = foldlWithIndex unpackLine Set.empty <$> unlines
    where 
        unlines :: Parser String (List (Array Boolean))
        unlines = P.sepBy1 (Array.many locParser) (P.char '\n')
        unpackLine :: Int -> Set Location -> Array Boolean -> Set Location
        unpackLine y s bs = foldlWithIndex (unpackLocation y) s bs
        unpackLocation :: Int -> Int -> Set Location -> Boolean -> Set Location
        unpackLocation y x s b  = if b then Set.insert (Tuple x y) $ s else s
