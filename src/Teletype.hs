{-# LANGUAGE 
    DerivingStrategies,
    DerivingVia,
    DeriveGeneric #-} 
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

module Teletype where

import Prelude

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

log :: Member Teletype r => String -> Sem r ()
log s = writeTTY s

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletypePure :: Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure
  = runOutputMonoid pure         --  For each WriteTTY in our program, consume an output by appending it to the list in a ([String], a)
  . runInputList ([]::[String])  -- Treat each element of our list of strings as a line of input
                                 -- {- Reinterpret our effect in terms of Input and Output -}
  . reinterpret2 \case           
      WriteTTY msg -> output msg
      ReadTTY -> maybe "" id <$> input
