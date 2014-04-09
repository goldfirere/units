{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, FlexibleInstances,
             ScopedTypeVariables, FlexibleContexts, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dimensions.Show
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines only a 'Show' instance for dimensioned quantities.
-- The Show instance prints out the number stored internally with its canonical
-- units.
-----------------------------------------------------------------------------

module Data.Dimensions.Show (showIn) where

import Data.Proxy (Proxy(..))
import Data.List
import Data.Singletons (Sing, sing, SingI)

import Data.Dimensions.DimSpec
import Data.Dimensions.Dim
import Data.Dimensions.Z
import Data.Dimensions.LCSU
import Data.Dimensions.UnitCombinators
import Data.Dimensions.Units
import Data.Dimensions

class ShowUnitSpec (dims :: [DimSpec *]) where
  showDims :: Proxy dims -> ([String], [String])

instance ShowUnitSpec '[] where
  showDims _ = ([], [])

instance (ShowUnitSpec rest, Show unit, SingI z)
         => ShowUnitSpec (D unit z ': rest) where
  showDims _ =
    let (nums, denoms) = showDims (Proxy :: Proxy rest)
        baseStr        = show (undefined :: unit)
        power          = szToInt (sing :: Sing z)
        abs_power      = abs power
        str            = if abs_power == 1
                         then baseStr
                         else baseStr ++ "^" ++ (show abs_power) in
    case compare power 0 of
      LT -> (nums, str : denoms)
      EQ -> (nums, denoms)
      GT -> (str : nums, denoms)

showDimSpec :: ShowUnitSpec dimspec => Proxy dimspec -> String
showDimSpec p
  = let (nums, denoms) = mapPair (build_string . sort) $ showDims p in
    case (length nums, length denoms) of
      (0, 0) -> ""
      (_, 0) -> " " ++ nums
      (0, _) -> " 1/" ++ denoms
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

instance (ShowUnitSpec (LookupList dims lcsu), Show n)
           => Show (Dim dims lcsu n) where
  show (Dim d) = (show d ++ showDimSpec (Proxy :: Proxy (LookupList dims lcsu)))

infix 1 `showIn`
-- | Show a dimensioned quantity in a given unit. (The default @Show@
-- instance always uses canonical units.)
showIn :: ( Unit unit
          , UnitSpec (LookupList dim lcsu)      
          , UnitSpecsOf unit *~ LookupList dim lcsu
          , Fractional n
          , Show unit
          , Show n )
       => Dim dim lcsu n -> unit -> String
showIn x u = show (x # u) ++ " " ++ show u
