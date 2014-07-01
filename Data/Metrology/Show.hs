{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, FlexibleInstances,
             ScopedTypeVariables, FlexibleContexts, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Show
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines 'Show' instance for quantities. The show instance
-- prints out the number stored internally with its canonical units. To print
-- out quantities with specific units use the function `showIn`.
-----------------------------------------------------------------------------

module Data.Metrology.Show (showIn) where

import Data.Proxy (Proxy(..))
import Data.List
import Data.Singletons (Sing, sing, SingI)

import Data.Metrology.Factor
import Data.Metrology.Quantity
import Data.Metrology.Z
import Data.Metrology.LCSU
import Data.Metrology.Combinators
import Data.Metrology

import Data.VectorSpace

class ShowUnitFactor (dims :: [Factor *]) where
  showDims :: Bool   -- take absolute value of exponents?
           -> Proxy dims -> ([String], [String])

instance ShowUnitFactor '[] where
  showDims _ _ = ([], [])

instance (ShowUnitFactor rest, Show unit, SingI z)
         => ShowUnitFactor (F unit z ': rest) where
  showDims take_abs _ =
    let (nums, denoms) = showDims take_abs (Proxy :: Proxy rest)
        baseStr        = show (undefined :: unit)
        power          = szToInt (sing :: Sing z)
        abs_power      = if take_abs then abs power else power
        str            = if abs_power == 1
                         then baseStr
                         else baseStr ++ "^" ++ (show abs_power) in
    case compare power 0 of
      LT -> (nums, str : denoms)
      EQ -> (nums, denoms)
      GT -> (str : nums, denoms)

showFactor :: ShowUnitFactor dimspec => Proxy dimspec -> String
showFactor p
  = let (nums, denoms) = mapPair (build_string . sort) $ showDims True p in
    case (length nums, length denoms) of
      (0, 0) -> ""
      (_, 0) -> " " ++ nums
      (0, _) -> build_string (snd (showDims False p))
      (_, _) -> " " ++ nums ++ "/" ++ denoms
  where
    mapPair :: (a -> b) -> (a, a) -> (b, b)
    mapPair f (x, y) = (f x, f y)

    build_string :: [String] -> String
    build_string [] = ""
    build_string [s] = s
    build_string s = "(" ++ build_string_helper s ++ ")"

    build_string_helper :: [String] -> String
    build_string_helper [] = ""
    build_string_helper [s] = s
    build_string_helper (h:t) = h ++ " * " ++ build_string_helper t

-- enable showing of compound units:
instance (Show u1, Show u2) => Show (u1 :* u2) where
  show _ = show (undefined :: u1) ++ " " ++ show (undefined :: u2)

instance (Show u1, Show u2) => Show (u1 :/ u2) where
  show _ = show (undefined :: u1) ++ "/" ++ show (undefined :: u2)

instance (Show u1, SingI power) => Show (u1 :^ (power :: Z)) where
  show _ = show (undefined :: u1) ++ "^" ++ show (szToInt (sing :: Sing power))

-- enable showing of units with prefixes:
instance (Show prefix, Show unit) => Show (prefix :@ unit) where
  show _ = show (undefined :: prefix) ++ show (undefined :: unit)

instance (ShowUnitFactor (LookupList dims lcsu), Show n)
           => Show (Qu dims lcsu n) where
  show (Qu d) = show d ++
                (' ' : showFactor (Proxy :: Proxy (LookupList dims lcsu)))

infix 1 `showIn`

-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses canonical units.)
showIn :: ( ValidDLU dim lcsu unit
          , VectorSpace n
          , Fractional (Scalar n)
          , Show unit
          , Show n )
       => Qu dim lcsu n -> unit -> String
showIn x u = show (x # u) ++ " " ++ show u
