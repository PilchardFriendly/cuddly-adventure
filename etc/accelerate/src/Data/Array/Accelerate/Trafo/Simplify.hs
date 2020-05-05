{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.Simplify
-- Copyright   : [2012..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.Simplify (

  Simplify(..),

) where

-- standard library
import Control.Applicative                              hiding ( Const )
import Control.Lens                                     hiding ( Const, ix )
import Data.List                                        ( nubBy )
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Text.Printf
import Prelude                                          hiding ( exp, iterate )

-- friends
import Data.Array.Accelerate.AST                        hiding ( prj )
import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Analysis.Shape
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Trafo.Algebra
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Shrink
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar                ( Array, Shape, Elt(..), Z(..), (:.)(..)
                                                        , Tuple(..), IsTuple, fromTuple, TupleRepr, shapeToList )
import qualified Data.Array.Accelerate.Debug.Stats      as Stats
import qualified Data.Array.Accelerate.Debug.Flags      as Debug
import qualified Data.Array.Accelerate.Debug.Trace      as Debug


class Simplify f where
  simplify :: f -> f

instance Kit acc => Simplify (PreFun acc aenv f) where
  simplify = simplifyFun

instance (Kit acc, Elt e) => Simplify (PreExp acc aenv e) where
  simplify = simplifyExp


-- Scalar optimisations
-- ====================

{--
-- Common subexpression elimination finds computations that are performed at
-- least twice on a given execution path and eliminates the second and later
-- occurrences, replacing them with uses of saved values. This implements a
-- simplified version of that idea, where we look for the expressions of the
-- form:
--
--   let x = e1 in e2
--
-- and replace all occurrences of e1 in e2 with x. This is not full redundancy
-- elimination, but good enough to catch some cases, and in particular those
-- likely to be introduced by scalar composition of terms in the fusion process.
--
-- While it may seem that common subexpression elimination is always worthwhile,
-- as it reduces the number of arithmetic operations performed, this is not
-- necessarily advantageous. The simplest case in which it may not be desirable
-- is if it causes a register to be occupied for a long time in order to hold
-- the shared expression's value, which hence reduces the number of registers
-- available for other uses. Even worse is if the value has to be spilled to
-- memory because there are insufficient registers available. We sidestep this
-- tricky and target-dependent issue by, for now, simply ignoring it.
--
localCSE :: (Kit acc, Elt a)
         => Gamma      acc env env aenv
         -> PreOpenExp acc env     aenv a
         -> PreOpenExp acc (env,a) aenv b
         -> Maybe (PreOpenExp acc env aenv b)
localCSE env bnd body
  | Just ix <- lookupExp env bnd = Stats.ruleFired "CSE" . Just $ inline body (Var ix)
  | otherwise                    = Nothing
--}
{--
-- Common subexpression elimination, which attempts to match the given
-- expression against something already bound in the environment. This can occur
-- due to simplification, in which case we replace the entire subterm with x.
--
-- > let x = e in .. e ..
--
globalCSE :: (Kit acc, Elt t)
          => Gamma      acc env env aenv
          -> PreOpenExp acc env     aenv t
          -> Maybe (PreOpenExp acc env aenv t)
globalCSE env exp
  | Just ix <- lookupExp env exp = Stats.ruleFired "CSE" . Just $ Var ix
  | otherwise                    = Nothing
--}

{--
-- Compared to regular Haskell, the scalar expression language of Accelerate is
-- rather limited in order to meet the restrictions of what can be efficiently
-- implemented on specialised hardware, such as GPUs. For example, to avoid
-- excessive SIMD divergence, we do not support any form of recursion or
-- iteration in scalar expressions. This harmonises well with the stratified
-- design of the Accelerate language: collective array operations comprise many
-- scalar computations that are executed in parallel, so for simplicity of
-- scheduling these operations we would like some assurance that each scalar
-- computation takes approximately the same time to execute as all others.
--
-- However, some computations are naturally expressed in terms of iteration. For
-- some problems, we can instead use generative techniques to implement the
-- program by defining a single step of a recurrence relation as an Accelerate
-- collective operation and using standard Haskell to unroll the loop a _fixed_
-- number of times.
--
-- However, this is outrageously slow because the intermediate values are
-- written to memory at the end of every iteration. Luckily the fusion process
-- will eliminate this intermediate memory traffic by combining the 'n'
-- collective operations into a single operation with 'n' instances of the loop
-- body. However, doing this we uncover an embarrassing secret: C compilers do
-- not compile C code, they compile _idiomatic_ C code.
--
-- This process recovers the iteration structure that was lost in the process of
-- fusing the collective operations. This allows a backend to generate explicit
-- loops in its target language.
--
recoverLoops
    :: (Kit acc, Elt b)
    => Gamma      acc env env aenv
    -> PreOpenExp acc env     aenv a
    -> PreOpenExp acc (env,a) aenv b
    -> Maybe (PreOpenExp acc env aenv b)
recoverLoops _ bnd e3
  -- To introduce scaler loops, we look for expressions of the form:
  --
  --   let x =
  --     let y = e1 in e2
  --   in e3
  --
  -- and if e2 and e3 are congruent, replace with:
  --
  --   iterate[2] (\y -> e2) e1
  --
  | Let e1 e2           <- bnd
  , Just Refl           <- matchEnvTop e2 e3
  , Just Refl           <- match e2 e3
  = Stats.ruleFired "loop recovery/intro" . Just
  $ Iterate (constant 2) e2 e1

  -- To merge expressions into a loop body, look for the pattern:
  --
  --   let x = iterate[n] f e1
  --   in e3
  --
  -- and if e3 matches the loop body, replace the let binding with the bare
  -- iteration with the trip count increased by one.
  --
  | Iterate n f e1      <- bnd
  , Just Refl           <- match f e3
  = Stats.ruleFired "loop recovery/merge" . Just
  $ Iterate (constant 1 `plus` n) f e1

  | otherwise
  = Nothing

  where
    plus :: PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv Int -> PreOpenExp acc env aenv Int
    plus x y = PrimApp (PrimAdd numType) $ Tuple $ NilTup `SnocTup` x `SnocTup` y

    constant :: Int -> PreOpenExp acc env aenv Int
    constant i = Const ((),i)

    matchEnvTop :: (Elt s, Elt t)
                => PreOpenExp acc (env,s) aenv f
                -> PreOpenExp acc (env,t) aenv g
                -> Maybe (s :=: t)
    matchEnvTop _ _ = gcast Refl
--}


-- Walk a scalar expression applying simplifications to terms bottom-up.
--
-- TODO: Look for particular patterns of expressions that can be replaced by
--       something equivalent and simpler. In particular, indexing operations
--       introduced by the fusion transformation. This would benefit from a
--       rewrite rule schema.
--
simplifyOpenExp
    :: forall acc env aenv e. (Kit acc, Elt e)
    => Gamma acc env env aenv
    -> PreOpenExp acc env aenv e
    -> (Bool, PreOpenExp acc env aenv e)
simplifyOpenExp env = first getAny . cvtE
  where
    cvtE :: Elt t => PreOpenExp acc env aenv t -> (Any, PreOpenExp acc env aenv t)
    cvtE exp = case exp of
      Let bnd body
        -- Just reduct <- recoverLoops env (snd bnd') (snd body') -> yes . snd $ cvtE reduct
        -- Just reduct <- localCSE     env (snd bnd') (snd body') -> yes . snd $ cvtE reduct
        | otherwise -> Let <$> bnd' <*> body'
        where
          bnd'  = cvtE bnd
          env'  = env `pushExp` snd bnd'
          body' = cvtE' (incExp env') body

      Var ix                    -> pure $ Var ix
      Const c                   -> pure $ Const c
      Undef                     -> pure Undef
      Tuple tup                 -> Tuple <$> cvtT tup
      Prj ix t                  -> prj env ix (cvtE t)
      IndexNil                  -> pure IndexNil
      IndexAny                  -> pure IndexAny
      IndexCons sh sz           -> indexCons (cvtE sh) (cvtE sz)
      IndexHead sh              -> indexHead (cvtE sh)
      IndexTail sh              -> indexTail (cvtE sh)
      IndexSlice x ix sh        -> IndexSlice x <$> cvtE ix <*> cvtE sh
      IndexFull x ix sl         -> IndexFull x <$> cvtE ix <*> cvtE sl
      ToIndex sh ix             -> toIndex (cvtE sh) (cvtE ix)
      FromIndex sh ix           -> fromIndex (cvtE sh) (cvtE ix)
      Cond p t e                -> cond (cvtE p) (cvtE t) (cvtE e)
      PrimConst c               -> pure $ PrimConst c
      PrimApp f x               -> (u<>v, fx)
        where
          (u, x') = cvtE x
          (v, fx) = evalPrimApp env f x'
      Index a sh                -> Index a <$> cvtE sh
      LinearIndex a i           -> LinearIndex a <$> cvtE i
      Shape a                   -> shape a
      ShapeSize sh              -> shapeSize (cvtE sh)
      Intersect s t             -> cvtE s `intersect` cvtE t
      Union s t                 -> cvtE s `union` cvtE t
      Foreign ff f e            -> Foreign ff <$> first Any (simplifyOpenFun EmptyExp f) <*> cvtE e
      While p f x               -> While <$> cvtF env p <*> cvtF env f <*> cvtE x
      Coerce e                  -> Coerce <$> cvtE e

    cvtT :: Tuple (PreOpenExp acc env aenv) t -> (Any, Tuple (PreOpenExp acc env aenv) t)
    cvtT NilTup        = pure NilTup
    cvtT (SnocTup t e) = SnocTup <$> cvtT t <*> cvtE e

    cvtE' :: Elt e' => Gamma acc env' env' aenv -> PreOpenExp acc env' aenv e' -> (Any, PreOpenExp acc env' aenv e')
    cvtE' env' = first Any . simplifyOpenExp env'

    cvtF :: Gamma acc env' env' aenv -> PreOpenFun acc env' aenv f -> (Any, PreOpenFun acc env' aenv f)
    cvtF env' = first Any . simplifyOpenFun env'

    -- Return the minimal set of unique shapes to intersect. This is a bit
    -- inefficient, but the number of shapes is expected to be small so should
    -- be fine in practice.
    --
    intersect :: Shape t
              => (Any, PreOpenExp acc env aenv t)
              -> (Any, PreOpenExp acc env aenv t)
              -> (Any, PreOpenExp acc env aenv t)
    intersect (c1, sh1) (c2, sh2)
      | Nothing <- match sh sh' = Stats.ruleFired "intersect" (yes sh')
      | otherwise               = (c1 <> c2, sh')
      where
        sh      = Intersect sh1 sh2
        sh'     = foldl1 Intersect
                $ nubBy (\x y -> isJust (match x y))
                $ leaves sh1 ++ leaves sh2

        leaves :: Shape t => PreOpenExp acc env aenv t -> [PreOpenExp acc env aenv t]
        leaves (Intersect x y)  = leaves x ++ leaves y
        leaves rest             = [rest]

    -- Return the minimal set of unique shapes to take the union of. This is a bit
    -- inefficient, but the number of shapes is expected to be small so should
    -- be fine in practice.
    --
    union :: Shape t
          => (Any, PreOpenExp acc env aenv t)
          -> (Any, PreOpenExp acc env aenv t)
          -> (Any, PreOpenExp acc env aenv t)
    union (c1, sh1) (c2, sh2)
      | Nothing <- match sh sh' = Stats.ruleFired "union" (yes sh')
      | otherwise               = (c1 <> c2, sh')
      where
        sh      = Union sh1 sh2
        sh'     = foldl1 Union
                $ nubBy (\x y -> isJust (match x y))
                $ leaves sh1 ++ leaves sh2

        leaves :: Shape t => PreOpenExp acc env aenv t -> [PreOpenExp acc env aenv t]
        leaves (Union x y)  = leaves x ++ leaves y
        leaves rest         = [rest]


    -- Simplify conditional expressions, in particular by eliminating branches
    -- when the predicate is a known constant.
    --
    cond :: forall t. Elt t
         => (Any, PreOpenExp acc env aenv Bool)
         -> (Any, PreOpenExp acc env aenv t)
         -> (Any, PreOpenExp acc env aenv t)
         -> (Any, PreOpenExp acc env aenv t)
    cond p@(_,p') t@(_,t') e@(_,e')
      | Const True  <- p'        = Stats.knownBranch "True"      (yes t')
      | Const False <- p'        = Stats.knownBranch "False"     (yes e')
      | Just Refl <- match t' e' = Stats.knownBranch "redundant" (yes e')
      | otherwise                = Cond <$> p <*> t <*> e

    -- If we are projecting elements from a tuple structure or tuple of constant
    -- valued tuple, pick out the appropriate component directly.
    --
    -- Follow variable bindings, but only if they result in a simplification.
    --
    prj :: forall env' s t. (Elt s, Elt t, IsTuple t)
        => Gamma acc env' env' aenv
        -> TupleIdx (TupleRepr t) s
        -> (Any, PreOpenExp acc env' aenv t)
        -> (Any, PreOpenExp acc env' aenv s)
    prj env' ix top@(_,e) = case e of
      Tuple t                      -> Stats.inline "prj/Tuple" . yes $ prjT ix t
      Const c                      -> Stats.inline "prj/Const" . yes $ prjC ix (fromTuple (toElt c :: t))
      Var v   | Just x <- prjV v   -> Stats.inline "prj/Var"   . yes $ x
      Let a b | Just x <- prjL a b -> Stats.inline "prj/Let"   . yes $ x
      _                            -> Prj ix <$> top
      where
        prjT :: TupleIdx tup s -> Tuple (PreOpenExp acc env' aenv) tup -> PreOpenExp acc env' aenv s
        prjT ZeroTupIdx       (SnocTup _ v) = v
        prjT (SuccTupIdx idx) (SnocTup t _) = prjT idx t
#if __GLASGOW_HASKELL__ < 800
        prjT _                _             = error "DO MORE OF WHAT MAKES YOU HAPPY"
#endif

        prjC :: TupleIdx tup s -> tup -> PreOpenExp acc env' aenv s
        prjC ZeroTupIdx       (_,   v) = Const (fromElt v)
        prjC (SuccTupIdx idx) (tup, _) = prjC idx tup

        prjV :: Idx env' t -> Maybe (PreOpenExp acc env' aenv s)
        prjV var
          | e'      <- prjExp var env'
          , Nothing <- match e e'
          = case e' of
              -- Don't push through nested let-bindings; this leads to code explosion
              Let _ _                                    -> Nothing
              _ | (Any True, x) <- prj env' ix (pure e') -> Just x
              _                                          -> Nothing
          | otherwise
          = Nothing

        prjL :: Elt a
             => PreOpenExp acc env'     aenv a
             -> PreOpenExp acc (env',a) aenv t
             -> Maybe (PreOpenExp acc env' aenv s)
        prjL a b
          | (Any True, c) <- prj (incExp $ pushExp env' a) ix (pure b) = Just (Let a c)
        prjL _ _                                                       = Nothing


    -- Shape manipulations
    --
    indexCons :: (Elt sl, Elt sz)
              => (Any, PreOpenExp acc env aenv sl)
              -> (Any, PreOpenExp acc env aenv sz)
              -> (Any, PreOpenExp acc env aenv (sl :. sz))
    indexCons (_,IndexNil) (_,Const c)
      | Just c'         <- cast c       -- EltRepr Z ~ EltRepr ()
      = Stats.ruleFired "Z:.const" $ yes (Const c')
    indexCons (_,IndexNil) (_,IndexHead sz')
      | 1               <- expDim sz'   -- no type information that this is a 1D shape, hence gcast next
      , Just sh'        <- gcast sz'
      = Stats.ruleFired "Z:.indexHead" $ yes sh'
    indexCons (_,IndexTail sl') (_,IndexHead sz')
      | Just Refl       <- match sl' sz'
      = Stats.ruleFired "indexTail:.indexHead" $ yes sl'
    indexCons sl sz
      = IndexCons <$> sl <*> sz

    indexHead :: forall sl sz. (Elt sl, Elt sz) => (Any, PreOpenExp acc env aenv (sl :. sz)) -> (Any, PreOpenExp acc env aenv sz)
    indexHead (_, Const c)
      | _ :. sz <- toElt c :: sl :. sz  = Stats.ruleFired "indexHead/const"     $ yes (Const (fromElt sz))
    indexHead (_, IndexCons _ sz)       = Stats.ruleFired "indexHead/indexCons" $ yes sz
    indexHead sh                        = IndexHead <$> sh

    indexTail :: forall sl sz. (Elt sl, Elt sz) => (Any, PreOpenExp acc env aenv (sl :. sz)) -> (Any, PreOpenExp acc env aenv sl)
    indexTail (_, Const c)
      | sl :. _ <- toElt c :: sl :. sz  = Stats.ruleFired "indexTail/const"     $ yes (Const (fromElt sl))
    indexTail (_, IndexCons sl _)       = Stats.ruleFired "indexTail/indexCons" $ yes sl
    indexTail sh                        = IndexTail <$> sh

    shape :: forall sh t. (Shape sh, Elt t) => acc aenv (Array sh t) -> (Any, PreOpenExp acc env aenv sh)
    shape _
      | Just Refl <- matchTupleType (eltType @sh) (eltType @Z)
      = Stats.ruleFired "shape/Z" $ yes (Const (fromElt Z))
    shape a
      = pure $ Shape a

    shapeSize :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int)
    shapeSize (_, Const c) = Stats.ruleFired "shapeSize/const" $ yes (Const (product (shapeToList (toElt c :: sh))))
    shapeSize sh           = ShapeSize <$> sh

    toIndex :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int)
    toIndex  (_,sh) (_,FromIndex sh' ix)
      | Just Refl <- match sh sh' = Stats.ruleFired "toIndex/fromIndex" $ yes ix
    toIndex sh ix                 = ToIndex <$> sh <*> ix

    fromIndex :: forall sh. Shape sh => (Any, PreOpenExp acc env aenv sh) -> (Any, PreOpenExp acc env aenv Int) -> (Any, PreOpenExp acc env aenv sh)
    fromIndex  (_,sh) (_,ToIndex sh' ix)
      | Just Refl <- match sh sh' = Stats.ruleFired "fromIndex/toIndex" $ yes ix
    fromIndex sh ix               = FromIndex <$> sh <*> ix

    first :: (a -> a') -> (a,b) -> (a',b)
    first f (x,y) = (f x, y)

    yes :: x -> (Any, x)
    yes x = (Any True, x)


-- Simplification for open functions
--
simplifyOpenFun
    :: Kit acc
    => Gamma acc env env aenv
    -> PreOpenFun acc env aenv f
    -> (Bool, PreOpenFun acc env aenv f)
simplifyOpenFun env (Body e) = Body <$> simplifyOpenExp env  e
simplifyOpenFun env (Lam f)  = Lam  <$> simplifyOpenFun env' f
  where
    env' = incExp env `pushExp` Var ZeroIdx


-- Simplify closed expressions and functions. The process is applied
-- repeatedly until no more changes are made.
--
simplifyExp :: (Elt t, Kit acc) => PreExp acc aenv t -> PreExp acc aenv t
simplifyExp = iterate summariseOpenExp (simplifyOpenExp EmptyExp)

simplifyFun :: Kit acc => PreFun acc aenv f -> PreFun acc aenv f
simplifyFun = iterate summariseOpenFun (simplifyOpenFun EmptyExp)


-- NOTE: [Simplifier iterations]
--
-- Run the simplification pass _before_ the shrinking step. There are cases
-- where it is better to run shrinking first, and then simplification would
-- complete in a single step, but the converse is also true. However, as
-- shrinking can remove some structure of the let bindings, which might be
-- useful for the transformations (e.g. loop recovery) we want to maintain this
-- information for at least the first pass.
--
-- We always apply the simplification step once. Following this, we iterate
-- shrinking and simplification until the expression no longer changes. Both
-- shrink and simplify return a boolean indicating whether any work was done; we
-- stop as soon as either returns false.
--
-- With internal checks on, we also issue a warning if the iteration limit is
-- reached, but it was still possible to make changes to the expression.
--
{-# SPECIALISE iterate :: (Exp aenv t -> Stats) -> (Exp aenv t -> (Bool, Exp aenv t)) -> Exp aenv t -> Exp aenv t #-}
{-# SPECIALISE iterate :: (Fun aenv t -> Stats) -> (Fun aenv t -> (Bool, Fun aenv t)) -> Fun aenv t -> Fun aenv t #-}

iterate
    :: forall f a. (Match f, Shrink (f a))
    => (f a -> Stats)
    -> (f a -> (Bool, f a))
    -> f a
    -> f a
iterate summarise f = fix 1 . setup
  where
    -- The maximum number of simplifier iterations. To be conservative and avoid
    -- excessive run times, we (should) set this value very low.
    --
    -- TODO: make this tunable via debug flags.
    --
    lIMIT       = 25

    simplify'   = Stats.simplifierDone . f
    setup x     = Debug.trace Debug.dump_simpl_iterations (msg 0 "init" x)
                $ snd (trace 1 "simplify" (simplify' x))

    fix :: Int -> f a -> f a
    fix i x0
      | i > lIMIT       = $internalWarning "simplify" "iteration limit reached" (not (x0 ==^ f x0)) x0
      | not shrunk      = x1
      | not simplified  = x2
      | otherwise       = fix (i+1) x2
      where
        (shrunk,     x1) = trace i "shrink"   $ shrink' x0
        (simplified, x2) = trace i "simplify" $ simplify' x1

    -- debugging support
    --
    u ==^ (_,v)         = isJust (match u v)

    trace i s v@(changed,x)
      | changed         = Debug.trace Debug.dump_simpl_iterations (msg i s x) v
      | otherwise       = v

    msg :: Int -> String -> f a -> String
    msg i s x = printf "simpl-iters/%-8s [%d]: %s" s i (ppr x)

    ppr :: f a -> String
    ppr = show . summarise


-- Debugging support
-- -----------------

data Stats = Stats
  { _terms    :: {-# UNPACK #-} !Int
  , _types    :: {-# UNPACK #-} !Int
  , _binders  :: {-# UNPACK #-} !Int
  , _vars     :: {-# UNPACK #-} !Int
  , _ops      :: {-# UNPACK #-} !Int
  }

instance Show Stats where
  show (Stats a b c d e) =
    printf "terms = %d, types = %d, lets = %d, vars = %d, primops = %d" a b c d e

infixl 6 +++
(+++) :: Stats -> Stats -> Stats
Stats a1 b1 c1 d1 e1 +++ Stats a2 b2 c2 d2 e2 = Stats (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2)
{-# INLINE (+++) #-}

terms, types, binders, vars, ops :: Lens' Stats Int
terms   = lens _terms   (\Stats{..} v -> Stats { _terms   = v, ..})
types   = lens _types   (\Stats{..} v -> Stats { _types   = v, ..})
binders = lens _binders (\Stats{..} v -> Stats { _binders = v, ..})
vars    = lens _vars    (\Stats{..} v -> Stats { _vars    = v, ..})
ops     = lens _ops     (\Stats{..} v -> Stats { _ops     = v, ..})
{-# INLINE terms   #-}
{-# INLINE types   #-}
{-# INLINE binders #-}
{-# INLINE vars    #-}
{-# INLINE ops     #-}

summariseOpenFun :: PreOpenFun acc env aenv f -> Stats
summariseOpenFun (Body e) = summariseOpenExp e & terms +~ 1
summariseOpenFun (Lam f)  = summariseOpenFun f & terms +~ 1 & binders +~ 1

summariseOpenExp :: PreOpenExp acc env aenv t -> Stats
summariseOpenExp = (terms +~ 1) . goE
  where
    zero = Stats 0 0 0 0 0

    travE :: PreOpenExp acc env aenv t -> Stats
    travE = summariseOpenExp

    travF :: PreOpenFun acc env aenv t -> Stats
    travF = summariseOpenFun

    travA :: acc aenv a -> Stats
    travA _ = zero & vars +~ 1  -- assume an array index, else we should have failed elsewhere

    travT :: Tuple (PreOpenExp acc env aenv) t -> Stats
    travT NilTup        = zero & terms +~ 1
    travT (SnocTup t e) = travT t +++ travE e & terms +~ 1

    travTix :: TupleIdx t e -> Stats
    travTix ZeroTupIdx     = zero & terms +~ 1
    travTix (SuccTupIdx t) = travTix t & terms +~ 1

    travC :: PrimConst c -> Stats
    travC (PrimMinBound t) = travBoundedType t & terms +~ 1
    travC (PrimMaxBound t) = travBoundedType t & terms +~ 1
    travC (PrimPi t)       = travFloatingType t & terms +~ 1

    travNonNumType :: NonNumType t -> Stats
    travNonNumType _ = zero & types +~ 1

    travIntegralType :: IntegralType t -> Stats
    travIntegralType _ = zero & types +~ 1

    travFloatingType :: FloatingType t -> Stats
    travFloatingType _ = zero & types +~ 1

    travNumType :: NumType t -> Stats
    travNumType (IntegralNumType t) = travIntegralType t & types +~ 1
    travNumType (FloatingNumType t) = travFloatingType t & types +~ 1

    travBoundedType :: BoundedType t -> Stats
    travBoundedType (IntegralBoundedType t) = travIntegralType t & types +~ 1
    travBoundedType (NonNumBoundedType t)   = travNonNumType t & types +~ 1

    -- travScalarType :: ScalarType t -> Stats
    -- travScalarType (SingleScalarType t) = travSingleType t & types +~ 1
    -- travScalarType (VectorScalarType t) = travVectorType t & types +~ 1

    travSingleType :: SingleType t -> Stats
    travSingleType (NumSingleType t)    = travNumType t & types +~ 1
    travSingleType (NonNumSingleType t) = travNonNumType t & types +~ 1

    -- travVectorType :: VectorType t -> Stats
    -- travVectorType (Vector2Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector3Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector4Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector8Type t)  = travSingleType t & types +~ 1
    -- travVectorType (Vector16Type t) = travSingleType t & types +~ 1

    -- The scrutinee has already been counted
    goE :: PreOpenExp acc env aenv t -> Stats
    goE exp =
      case exp of
        Let bnd body          -> travE bnd +++ travE body & binders +~ 1
        Var{}                 -> zero & vars +~ 1
        Foreign _ _ x         -> travE x & terms +~ 1   -- +1 for asm, ignore fallback impls.
        Const{}               -> zero
        Undef                 -> zero
        Tuple tup             -> travT tup & terms +~ 1
        Prj ix e              -> travTix ix +++ travE e
        IndexNil              -> zero
        IndexCons sh sz       -> travE sh +++ travE sz
        IndexHead sh          -> travE sh
        IndexTail sh          -> travE sh
        IndexAny              -> zero
        IndexSlice _ slix sh  -> travE slix +++ travE sh & terms +~ 1 -- +1 for sliceIndex
        IndexFull _ slix sl   -> travE slix +++ travE sl & terms +~ 1 -- +1 for sliceIndex
        ToIndex sh ix         -> travE sh +++ travE ix
        FromIndex sh ix       -> travE sh +++ travE ix
        Cond p t e            -> travE p +++ travE t +++ travE e
        While p f x           -> travF p +++ travF f +++ travE x
        PrimConst c           -> travC c
        Index a ix            -> travA a +++ travE ix
        LinearIndex a ix      -> travA a +++ travE ix
        Shape a               -> travA a
        ShapeSize sh          -> travE sh
        Intersect sh1 sh2     -> travE sh1 +++ travE sh2
        Union sh1 sh2         -> travE sh1 +++ travE sh2
        PrimApp f x           -> travPrimFun f +++ travE x
        Coerce e              -> travE e

    travPrimFun :: PrimFun f -> Stats
    travPrimFun = (ops +~ 1) . goF
      where
        goF :: PrimFun f -> Stats
        goF fun =
          case fun of
            PrimAdd                t -> travNumType t
            PrimSub                t -> travNumType t
            PrimMul                t -> travNumType t
            PrimNeg                t -> travNumType t
            PrimAbs                t -> travNumType t
            PrimSig                t -> travNumType t
            PrimQuot               t -> travIntegralType t
            PrimRem                t -> travIntegralType t
            PrimQuotRem            t -> travIntegralType t
            PrimIDiv               t -> travIntegralType t
            PrimMod                t -> travIntegralType t
            PrimDivMod             t -> travIntegralType t
            PrimBAnd               t -> travIntegralType t
            PrimBOr                t -> travIntegralType t
            PrimBXor               t -> travIntegralType t
            PrimBNot               t -> travIntegralType t
            PrimBShiftL            t -> travIntegralType t
            PrimBShiftR            t -> travIntegralType t
            PrimBRotateL           t -> travIntegralType t
            PrimBRotateR           t -> travIntegralType t
            PrimPopCount           t -> travIntegralType t
            PrimCountLeadingZeros  t -> travIntegralType t
            PrimCountTrailingZeros t -> travIntegralType t
            PrimFDiv               t -> travFloatingType t
            PrimRecip              t -> travFloatingType t
            PrimSin                t -> travFloatingType t
            PrimCos                t -> travFloatingType t
            PrimTan                t -> travFloatingType t
            PrimAsin               t -> travFloatingType t
            PrimAcos               t -> travFloatingType t
            PrimAtan               t -> travFloatingType t
            PrimSinh               t -> travFloatingType t
            PrimCosh               t -> travFloatingType t
            PrimTanh               t -> travFloatingType t
            PrimAsinh              t -> travFloatingType t
            PrimAcosh              t -> travFloatingType t
            PrimAtanh              t -> travFloatingType t
            PrimExpFloating        t -> travFloatingType t
            PrimSqrt               t -> travFloatingType t
            PrimLog                t -> travFloatingType t
            PrimFPow               t -> travFloatingType t
            PrimLogBase            t -> travFloatingType t
            PrimTruncate         f i -> travFloatingType f +++ travIntegralType i
            PrimRound            f i -> travFloatingType f +++ travIntegralType i
            PrimFloor            f i -> travFloatingType f +++ travIntegralType i
            PrimCeiling          f i -> travFloatingType f +++ travIntegralType i
            PrimIsNaN              t -> travFloatingType t
            PrimIsInfinite         t -> travFloatingType t
            PrimAtan2              t -> travFloatingType t
            PrimLt                 t -> travSingleType t
            PrimGt                 t -> travSingleType t
            PrimLtEq               t -> travSingleType t
            PrimGtEq               t -> travSingleType t
            PrimEq                 t -> travSingleType t
            PrimNEq                t -> travSingleType t
            PrimMax                t -> travSingleType t
            PrimMin                t -> travSingleType t
            PrimLAnd                 -> zero
            PrimLOr                  -> zero
            PrimLNot                 -> zero
            PrimOrd                  -> zero
            PrimChr                  -> zero
            PrimBoolToInt            -> zero
            PrimFromIntegral     i n -> travIntegralType i +++ travNumType n
            PrimToFloating       n f -> travNumType n +++ travFloatingType f

