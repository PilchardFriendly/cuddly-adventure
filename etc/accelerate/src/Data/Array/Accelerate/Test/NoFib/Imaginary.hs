{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Array.Accelerate.Test.NoFib.Imaginary
-- Copyright   : [2009..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Test.NoFib.Imaginary (

  test_imaginary,

  module Data.Array.Accelerate.Test.NoFib.Imaginary.SASUM,
  module Data.Array.Accelerate.Test.NoFib.Imaginary.SAXPY,
  module Data.Array.Accelerate.Test.NoFib.Imaginary.DotP,

) where

import Test.Tasty

import Data.Array.Accelerate.Test.NoFib.Base
import Data.Array.Accelerate.Test.NoFib.Imaginary.SASUM
import Data.Array.Accelerate.Test.NoFib.Imaginary.SAXPY
import Data.Array.Accelerate.Test.NoFib.Imaginary.DotP


test_imaginary :: RunN -> TestTree
test_imaginary runN =
  testGroup "imaginary"
    [ test_sasum runN
    , test_saxpy runN
    , test_dotp runN
    ]

