{- Regression test for #23 -}

{-# LANGUAGE TypeOperators, ConstraintKinds, FlexibleContexts #-}

module Tests.Compile.T23 where

import Data.Metrology.Poly
import Data.VectorSpace

ratio :: (d1 @~ d2, VectorSpace n, Fractional (Scalar n), Fractional n)
      => Qu d1 l n -> Qu d2 l n -> n
ratio x y = (x |/| y) # Number
