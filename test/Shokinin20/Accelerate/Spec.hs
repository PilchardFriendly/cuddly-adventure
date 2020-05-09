{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns          #-}

module Shokinin20.Accelerate.Spec (spec) where
import Prelude hiding (zipWith)
import Data.Kind
import Test.Hspec
import Test.Hspec.Expectations
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart as S
import Data.Array.Accelerate (Stencil3x3, stencil, Exp(..))
import Data.Array.Accelerate.Product ()
-- import Data.Array.Accelerate.Pattern as AP
import Data.Array.Accelerate.Data.Semigroup (Min(..))
import Data.Array.Accelerate.Interpreter  as Interpreter
import Data.Array.Accelerate.LLVM.Native  as CPU
import Prelude as P

type Stencil3x3x3x3 a = (Stencil3x3x3 a, Stencil3x3x3 a, Stencil3x3x3 a)


spec :: Spec
spec = describe "Stepping" do
    describe "plus'"
        let
            step1 :: Acc (A.Vector  L) -> Acc (A.Vector L)
            step1 = A.map (plus' lOne)
        in do
            it "one" 
                let
                    !go = Interpreter.runN (step1 $ A.use $ A.fromList (Z:.3) [L 0, L 1, L 2])
                in do
                    (toList $ go) `shouldBe` (L <$> [100, 2, 3])
    describe "lmin"
        let
            step1 :: Acc (A.Vector L) -> Acc (A.Vector  L)-> Acc (A.Vector L)
            step1 as bs = A.zipWith lmin as bs
        in do
            it "all" 
                let
                    as = A.use $ A.fromList (Z:.3) [L 0, L 1, L 100]
                    bs = A.use $ A.fromList (Z:.3) [L 1, L 1, L 1]
                    !go = Interpreter.runN (step1 as bs)
                in do
                    (toList $ go) `shouldBe` (L <$> [0, 1, 1])

    describe "stepping" do
        it "should calculate starting position" 
            let
                !go = Interpreter.runN sample1x1x3x3 
            in do
                (toList go) `shouldBe` (L <$> [0,100,100,100,1,100,100,100,0])
        it "should calculate first step position" 
            let
                !go = Interpreter.runN (stepNPXY sample1x1x3x3)
            in do
                (toList go) `shouldBe` (L <$> [0,2,100,2,1,2,100,2,0])


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

newtype L = L Word8
  deriving (Show, Generic, Elt, P.Eq) 

instance IsProduct Elt L


pattern Top :: S.Exp L
pattern Top = S.Exp (S.Const (L 100))

pattern Bot :: S.Exp L
pattern Bot = S.Exp (S.Const (L 0))

pattern L_ :: Exp Word8 -> Exp L
pattern L_ x = Pattern (x)

-- {-# COMPLETE L_ #-}

isTop :: Exp L -> Exp Bool
isTop (L_ x) = (x A.== 100)

isBottom :: Exp L -> Exp Bool
isBottom (L_ x) = (x A.== 0)

lOne :: Exp L
lOne = S.Exp (S.Const (L 1))

plus :: Exp L -> Exp L -> Exp L
plus (L_ x) (L_ y) = L_ (100 `A.min` x + y)


plus' :: Exp L -> Exp L -> Exp L
plus' a b = A.cond 
                    (isTop a) Top (cond (isTop b) Top 
                    (cond (isBottom a) (cond (isBottom b) Bot Top)
                    (cond (isBottom b) Top (plus a b))))

lmin :: Exp L -> Exp L -> Exp L
lmin (L_ x) (L_ y) = L_ (x `A.min` y)

liftStencil :: forall a . Elt a => (Stencil3x3 a -> Exp a)
            -> Boundary (Array DIM4 a)
            -> Acc (Array DIM4 a)
            -> Acc (Array DIM4 a)
liftStencil = stencil . lift2DStencil

lift2DStencil :: (Stencil3x3 a-> exp b) -> (Stencil3x3x3x3 a -> exp b)
lift2DStencil f (_,(_,x,_),_) = f x

liftSmartStencil :: forall 
        stencil2
        exp 
        acc 
        a
        b. 
    (Elt a, Elt b) 
    => PreBoundary acc exp (Array DIM4 a)
    -> (Stencil3x3 a-> exp b) 
    -> acc (Array DIM4 a) 
    -> S.PreSmartAcc acc exp (Array DIM4 b)
liftSmartStencil b s = S.Stencil (lift2DStencil s) b

-- thing = smartLiftStencil Bot stepXY

crossStep :: forall a. (Exp a -> Exp a -> Exp a) -> (Exp a -> Exp a) -> Stencil3x3 a -> Exp a
crossStep f m ((_,a',_),(d',o',b'),(_,c',_)) = P.foldl f o' (P.map m vec)
    where
        vec :: [Exp a]
        vec = [a', b', c', d']

stepXY :: Stencil3x3 L -> Exp L
stepXY = crossStep lmin (plus' lOne)

stepNPXY :: Acc (A.Array DIM4 L) -> A.Acc (A.Array DIM4 L)
stepNPXY = liftStencil stepXY  (A.function $ A.const Bot) 


sample1x1x3x3 :: Acc (A.Array DIM4 L)
sample1x1x3x3 = A.generate 
    (A.constant ( Z :. 1 :. 1 :. 3 :. 3))
    (\ix -> let z0 = startingPoint ix
             in z0)
        where 
            startingPoint :: Exp DIM4 -> Exp L
            startingPoint ( Z_ ::. p ::. n ::. x ::. y) = start x y
            start :: Exp Int -> Exp Int -> Exp L
            start x y = cond (x A.== y) (cond (x A.== 1) lOne Bot) Top
            
