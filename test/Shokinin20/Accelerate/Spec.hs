{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell #-}

module Shokinin20.Accelerate.Spec (spec) where
import Prelude hiding (zipWith)
import Test.Hspec
import Test.Hspec.Expectations
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart as S
import Data.Array.Accelerate (Stencil3x3, stencil, Exp(..))
-- import Data.Array.Accelerate.Pattern as Pattern
import Data.Array.Accelerate.Type (TupleType(..))
-- import Data.Array.Accelerate.Product (ProdR(..), TupleIdx(..))
-- import Data.Array.Accelerate.Array.Sugar (Elt(..), TupleRepr(..))
import Data.Array.Accelerate.Data.Semigroup (Min(..))
import Prelude as P

type Stencil3x3x3x3 a = (Stencil3x3x3 a, Stencil3x3x3 a, Stencil3x3x3 a)

spec :: Spec
spec = describe "dotp" do
    it "should calculate dot product"
        pending    


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

newtype L = L Word8
  deriving (Show, Generic, Elt)
deriving instance (IsProduct Elt Word8) => IsProduct Elt L

pattern Top :: S.Exp L
pattern Top = S.Exp (S.Const (L 100))

pattern Bot :: S.Exp L
pattern Bot = S.Exp (S.Const (L 0))

pattern L_ :: S.Exp Word8 -> S.Exp L
pattern L_ x = Pattern x
{-# COMPLETE L_ #-}

isTop :: Exp L -> Exp Bool
isTop Top = S.Exp (S.Const True)
isTop _ = S.Exp (S.Const False)

isBottom :: Exp L -> Exp Bool
isBottom Bot = S.Exp (S.Const True)
isBottom _ = S.Exp (S.Const False)

lOne :: Exp L
lOne = S.Exp (S.Const (L 1))

plus :: Exp L -> Exp L -> Exp L
plus (L_ x) (L_ y) = L_ (100 `A.min` (x + y))


plus' :: Exp L -> Exp L -> Exp L
plus' a b = cond 
                (isTop a) Top 
                (cond 
                    (isTop b) Top 
                    (cond 
                        (isBottom a) (cond (isBottom b) Bot Top)
                        (plus a b)))

lmin :: Exp L -> Exp L -> Exp L
lmin (L_ x) (L_ y) = L_ (x `A.min` y)

subStencil :: forall a . Elt a => (Stencil3x3 a -> Exp a)
            -> Boundary (Array DIM4 a)
            -> Acc (Array DIM4 a)
            -> Acc (Array DIM4 a)
subStencil s = stencil s'
        where
            s' :: Stencil3x3x3x3 a -> Exp a
            s' (_,(_,x,_),_) = s x

step :: forall a. (Exp a -> Exp a -> Exp a) -> (Exp a -> Exp a) -> Stencil3x3 a -> Exp a
step f m ((_,a',_),(d',o',b'),(_,c',_)) = P.foldl f o' (P.map m vec)
    where
        vec :: [Exp a]
        vec = [a', b', c', d']

stepXY :: Stencil3x3 L -> Exp L
stepXY = step lmin (plus' lOne)

stepNPXY :: Acc (Array DIM4 L) -> Acc (Array DIM4 L)
stepNPXY = subStencil stepXY boundary
    where
         boundary = (A.function (P.const Bot))