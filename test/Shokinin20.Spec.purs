module Shokinin20.Spec(spec) where

import Prelude

import Control.Alt ((<|>))
import Control.Biapply (biapply)
import Control.Monad.Gen (class MonadGen, choose, chooseInt, elements, resize, unfoldable)
import Control.Monad.Gen.Common (genMaybe')
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (range, replicate, many) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldr, any)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List)
import Data.List (catMaybes, concatMap, filter, fromFoldable, range) as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Set (Set)
import Data.Set (delete, empty, fromFoldable, insert, member, toUnfoldable) as Set
import Data.String (joinWith)
import Data.String.Utils (stripMargin)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Test.QuickCheck (class Arbitrary, (<?>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldEndWith, shouldStartWith)
import Test.Spec.QuickCheck (quickCheck)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1) as P
import Text.Parsing.Parser.String (char) as P

type Location = Tuple Int Int

data Office = Office {
    officeStartingColumn :: Int
    ,officeSpaces :: Set Location  -- keyed by tuple e.g. (1,5)  
}

office :: Set Location -> Int -> Office
office spaces startingColumn = Office { 
        officeStartingColumn: startingColumn
        ,officeSpaces: Set.insert (Tuple startingColumn topY) spaces
}

-- Probabilty is a float between 0 and 1

newtype Probability = Probability Number
instance boundedProbability :: Bounded Probability where
    top = wrap 1.0
    bottom = wrap 0.0
derive instance newtypeProbability :: Newtype Probability _
derive instance eqProbability :: Eq Probability
derive instance ordProbability :: Ord Probability

-- genMaybeP :: forall m a. MonadGen m => Probability -> a -> m (Maybe a)
-- genMaybeP p a = genMaybe' (unwrap p) (pure a)

genObstacles :: forall m. MonadGen m => Probability -> List Location -> m (Set Location)
genObstacles prob xs = Set.fromFoldable <<< List.catMaybes <$> traverse sample xs
    where
        sample :: Location -> m (Maybe Location)
        sample a = genMaybe' (unwrap prob) (pure a)

genStartingColumn :: forall m. MonadGen m => m Int
genStartingColumn = chooseInt 0 topX

topY :: Int
topY = 9
topX :: Int
topX = 9

allLocations :: List Location
allLocations = do
    x <- List.range 0 topX
    y <- List.range 0 topY
    pure $ Tuple x y

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


{-- officeHasPath: True if there is a path from (n,topY)-> (m, 0) where n is the starting position--}
officeHasPath :: Office -> Boolean
{-- Using connectedComponents is brute force - since it will find all possible paths, effectively.
    It gets cheaper the more full of obstacles the office becomes
    but it's a useful library to get started with--}
officeHasPath o@(Office { officeStartingColumn, officeSpaces}) = any containsStartAndEnd $ Graph.vertices <$> (Graph.connectedComponents $ officeGraph officeSpaces)
    where
        containsStartAndEnd :: List Location -> Boolean
        containsStartAndEnd locations = any isExit locations && any isStart locations
        isExit :: Location -> Boolean
        isExit t = 0 == snd t 
        isStart :: Location -> Boolean
        isStart t = t == Tuple officeStartingColumn topY

{-- looks up, down, left, and right--}
possibleNeighbours :: Location -> List Location
possibleNeighbours l = sequence (List.fromFoldable offsets) l
    where
        offsets = biapply <$>[Tuple inc identity, Tuple dec identity, Tuple identity inc, Tuple identity dec]
        inc :: Int -> Int
        inc = (+) 1
        dec :: Int -> Int
        dec = flip (-) 1

type FoldR a b = a -> b -> b
type Builder a = a -> a
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
        neighbouringEdges l =  Tuple l <$> (List.filter isSpace $ possibleNeighbours l)
        allEdges :: List (Tuple Location Location)
        allEdges = List.concatMap neighbouringEdges allVertices

        
        buildAllEdges :: Builder (Graph Location Int)
        buildAllEdges g = foldr edge g allEdges
        edge :: FoldR (Tuple Location Location) (Graph Location Int)
        edge t g = (uncurry Graph.insertEdge) t 1 g 
        buildAllVertices :: Builder (Graph Location Int)
        buildAllVertices g = foldr Graph.insertVertex g allVertices


noiseGen :: forall m. MonadRec m => MonadGen m => m (List Location)
noiseGen = resize (\size -> min size 9) $ unfoldable locationGen
    where
        locationGen = do
            x <- chooseInt 0 topX
            y <- chooseInt 0 (topY-1)
            pure $ Tuple x y

newtype Solvable = Solvable Office
derive instance newtypeSolvable :: Newtype Solvable _
instance arbitrarySolvable :: Arbitrary Solvable where
  arbitrary = solvableGen

solvableGen :: forall m. MonadRec m => MonadGen m => m Solvable
solvableGen = do
    noise <- noiseGen
    startingColumn <- genStartingColumn
    pure $ wrap $ office (withFewerSpaces noise) startingColumn
    where 
        withFewerSpaces = foldr Set.delete (Set.fromFoldable allLocations)

newtype Unsolvable = Unsolvable Office
derive instance newtypeUnsolvable :: Newtype Unsolvable _
instance arbitraryUnsolvable :: Arbitrary Unsolvable where
  arbitrary = unsolvableGen

noisyUnsolvableGen :: forall m. MonadRec m => MonadGen m => m Unsolvable
noisyUnsolvableGen = do
    noise <- noiseGen
    startingColumn <- genStartingColumn
    pure $ wrap $ office (withMoreSpaces noise) startingColumn
     where withMoreSpaces = Set.fromFoldable
knownUnsolvableGen:: forall m. MonadRec m => MonadGen m => m Unsolvable
knownUnsolvableGen = do
    startingColumn <- genStartingColumn
    known <- parseMap <$> elements knownBadPatterns
    case known of
      Right spaces -> pure $ wrap $ office spaces startingColumn
      Left _ -> noisyUnsolvableGen

unsolvableGen :: forall m. MonadRec m => MonadGen m => m Unsolvable
unsolvableGen = choose knownUnsolvableGen noisyUnsolvableGen

knownBadPatterns :: (NonEmpty Array) String
knownBadPatterns = stripMargin <$> """  |O.........
                                        |.O........
                                        |..O.......
                                        |...O......
                                        |....O.....
                                        |.....O....
                                        |......O...
                                        |.......O..
                                        |........OO
                                        |..........""" :| []


spec :: Spec Unit
spec = describe "Shokinin 20" 
    let emptyMap = Set.fromFoldable allLocations
        fullMap = Set.empty in do

    describe "pathfinding" 
        let emptyOffice = office emptyMap 5
            fullOffice = office fullMap 1
        in do 
        it "empty map" do
            officeHasPath emptyOffice `shouldEqual` true
        it "full map" do
            officeHasPath fullOffice `shouldEqual` false

        describe "properties" do
            it "empty maps will always have a path even if you take away some spaces " do
              quickCheck \(Solvable o@(Office {officeSpaces})) -> officeHasPath o <?> ("Failed for:\n" <> renderMap officeSpaces)
            it "full maps will never have a path even if you take away some obstacles" do
              quickCheck \(Unsolvable o@(Office {officeSpaces})) -> (not $ officeHasPath o) <?> ("Failed for:\n" <> renderMap officeSpaces)

        describe "officeGraph" do
            describe "2 pt space (A<->B)" let 
                ptA = Tuple 1 1
                ptB = Tuple 1 0
                input = Set.fromFoldable $ [ptA, ptB]
                actual = officeGraph input
                expected = (Graph.insertEdge ptA ptB 1
                    <<< Graph.insertEdge ptB ptA 1 
                    <<< Graph.insertVertex ptA
                    <<< Graph.insertVertex ptB) Graph.empty
                in do
                it "should have 2 allVertices" do
                    Graph.vertices actual `shouldEqual` Graph.vertices expected
                it "should have A->B edges" do
                    (Graph.isAdjacent ptA ptB actual) `shouldEqual` Graph.isAdjacent ptA ptB expected
                it "should have B->A edges" do
                    (Graph.isAdjacent ptA ptB actual) `shouldEqual` Graph.isAdjacent ptA ptB expected

        describe "neightbours" do
            describe "single point"  
                let
                    pt = Tuple 1 10
                in do
                    it "should have 4 possibleNeighbours" do
                        possibleNeighbours pt `shouldEqual`  List.fromFoldable [Tuple 2 10, Tuple 0 10, Tuple 1 11, Tuple 1 9]

    describe "rendering/parsing" do
        describe "empty map" 
            let input = emptyMap
                actual = renderMap input
                expected = joinWith "\n" (Array.replicate 10 "..........")
                parsed = parseMap expected
            in do
                it "renderMap" do
                    actual `shouldEqual` expected
                it "parseMap" do
                    parsed `shouldEqual` Right input

        describe "full map"
            let input = fullMap
                actual = renderMap input
                parsed = parseMap actual
            in do
                it "renderMap" do
                    actual `shouldEqual` joinWith "\n" (Array.replicate 10 "OOOOOOOOOO")
                it "parseMap" do
                    parsed `shouldEqual` Right input

        describe "top half row"
            let input =  Set.fromFoldable $ (flip Tuple) 0 <$> Array.range 0 4
            in do
                it "renders map" do
                    renderMap input `shouldStartWith` ".....OOOOO\nO"
                it "parses rendered map" do
                    (parseMap $ renderMap input) `shouldEqual` (Right input)                        
        describe "bottom half column"
            let input =  Set.fromFoldable $ Tuple 9 <$> Array.range 5 9
            in do
                it "renders map" do
                    renderMap input `shouldEndWith` ( stripMargin """ |O
                                                             |OOOOOOOOOO
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.
                                                             |OOOOOOOOO.""")
                it "parses rendered map" do
                    (parseMap $ renderMap input) `shouldEqual` (Right input)
