module Control.Monad.Logic where

import Prelude
import Control.Alt (class Alt)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.MonadZero (class Alternative, class MonadZero, class Plus)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Newtype (class Newtype, unwrap)

{-- 
 \$ sk fk = ....  
  sk :: (a -> mr-> mr)
  fk :: m r
 sk means success kleisli and fk means failure kleisli (effectively unit -> m r)
 and so most of the control flows take those two kleisli and dump the fk into 
 --}

newtype LogicT r m a
  = LogicT ((a -> m r -> m r) -> m r -> m r)

derive instance genericLogicT :: Generic (LogicT r m a) _

derive instance newtypeLogicT :: Newtype (LogicT r m a) _

type Logic r
  = LogicT r Identity

instance semigroupLogicT :: Semigroup (LogicT r m a) where
  append (LogicT m1) (LogicT m2) = LogicT $ \sk fk -> m1 sk (m2 sk fk)

-- sconcat = foldr1 mplus
instance monadTransLogicT :: MonadTrans (LogicT r) where
  lift m = LogicT $ \sk fk -> m >>= \a -> sk a fk

instance functorLogicT :: Functor (LogicT r m) where
  map f lt = LogicT $ \sk fk -> (unwrap lt) (sk <<< f) fk

instance applicativeLogicT :: Applicative (LogicT r m) where
  pure a = LogicT $ \sk fk -> sk a fk

instance altLogicT :: Alt (LogicT r m) where
  -- empty = LogicT $ \_ fk -> fk
  alt f1 f2 = LogicT $ \sk fk -> (unwrap f1) sk ((unwrap f2) sk fk)

instance plusLogicT :: Plus (LogicT r m) where
  empty = mempty

instance alternativeLogicT :: Plus (LogicT r m) => Alternative (LogicT r m)

instance applyLogic :: Apply (LogicT r m) where
  apply f a = LogicT $ \sk fk -> (unwrap f) (\g fk' -> (unwrap a) (sk <<< g) fk') fk

instance monadLogicT :: Bind (LogicT r m) where
  bind m f = LogicT $ \sk fk -> (unwrap m) (\a fk' -> (unwrap (f a)) sk fk') fk

instance monadZeroLogicT :: (Plus (LogicT r m), Monad (LogicT r m)) => MonadZero (LogicT r m)

instance monoidLogicT :: Monoid (LogicT r m a) where
  mempty = LogicT $ \_ fk -> fk
