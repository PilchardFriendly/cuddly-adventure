module Teletype where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Tuple (Tuple)
import Data.Unfoldable1 (class Unfoldable1, singleton)
import Effect (Effect)
import Effect.Class.Console as ConsoleEff

{-- Teletype is a way to mock console interactions.  Use `runTeletype` for the "real" one.  Use `writeTeletype` to get a writer output --}
data TeletypeF a = 
    Log String a
derive instance functorConsoleF :: Functor TeletypeF
type Teletype = Free TeletypeF
log :: String -> Teletype Unit
log s = liftF (Log s unit)

runTeletypeF :: TeletypeF ~> Effect
runTeletypeF (Log s next) = do
      ConsoleEff.log s
      pure next
runTeletype :: Teletype ~> Effect
runTeletype = foldFree runTeletypeF

{-- Log writes since strings at once.  So as long as the we can create a singleton (Unfoldable1), we can have a writer--}
writeTeletypeF :: forall f. Monoid (f String) => Unfoldable1 f => TeletypeF ~> Writer (f String)
writeTeletypeF (Log s next)= do
        tell $ singleton s
        pure next

writeTeletype :: forall a f. Monoid (f String) => Unfoldable1 f => Teletype a -> Tuple a (f String)
writeTeletype program = runWriter (foldFree writeTeletypeF program)
