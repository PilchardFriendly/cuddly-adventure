{-# LANGUAGE BlockArguments, RankNTypes #-}
module Shokinin20.Rendering where

import Prelude

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Either (Either)
-- import Data.List.Index (ifoldl)
import Data.Set (Set)
import Data.Void
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.List.Index (ifoldl)
import Shokinin20.Types (Location, Office(..))
import Shokinin20.Internal (topX, topY)
import Text.Megaparsec (Parsec, ParsecT, MonadParsec, runParser, runParserT, sepBy1, many, (<?>))
import Text.Megaparsec.Stream (Token)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Text.Megaparsec.Char as P

type Parser a = ParsecT String String Identity a
renderLocation ::  Set Location -> Location -> String
renderLocation locations location = if Set.member location locations then "." else "O"


renderLine' :: (Location -> String)  -> Int -> [Int] -> String
renderLine' f y xs = intercalate "" go
    where
        go = do
            x <- xs
            pure $ f (x,y)

renderMap :: Set Location -> String
renderMap locations = renderMap' (renderLocation locations) locations


renderMap' ::  (Location -> String) -> Set Location -> String
renderMap' f locations = intercalate "\n" $ do
    y <- [0..topY]
    pure $ renderLine' f y [0..topX]

renderOffice :: Office -> String
renderOffice o@(Office _ spaces) = renderMap spaces

parseMap :: String -> Either String (Set Location)
parseMap s = runIdentity $ first (const "Argh") <$> (runParserT p "Map content" s)
    where
        p :: Parser (Set Location)
        p = (mapParser locationParser)

locationParser :: (MonadParsec e s m, Token s ~ Char) => m Bool
locationParser = ((const True) <$> P.char '.') <|> ((const False <$> P.char 'O'))
-- locationParser = undefined 

mapParser :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m Bool -> m (Set Location)
mapParser locParser = (ifoldl (flip unpackLine) Set.empty) <$> unlines
    where
        unlines :: m [[Bool]]
        unlines = sepBy1 (many locParser) (P.char '\n')
        unpackLine :: Int -> Set Location -> [Bool] -> Set Location
        unpackLine y = ifoldl (flip $ unpackLocation y)
        unpackLocation :: Int -> Int -> Set Location -> Bool -> Set Location
        unpackLocation y x s b  = if b then Set.insert (x,y) s else s
