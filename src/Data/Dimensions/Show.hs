{- Data/Dimensions/Show.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This file defines Show instances for dimensioned quantities.
-}

{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, FlexibleInstances,
             ScopedTypeVariables #-}

-- | This module defines only a @Show@ instance for dimensioned quantities.
-- The Show instance prints out the number stored internally with its canonical
-- units.

module Data.Dimensions.Show () where

import Data.Proxy (Proxy(..))
import Data.List
import Data.Singletons (Sing, sing, SingI)

import Data.Dimensions.DimSpec
import Data.Dimensions.Dim
import Data.Dimensions.Z

class ShowDimSpec (dims :: [DimSpec *]) where
  showDims :: Proxy dims -> ([String], [String])

instance ShowDimSpec '[] where
  showDims _ = ([], [])

instance (ShowDimSpec rest, Show unit, SingI z)
         => ShowDimSpec (D unit z ': rest) where
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

showDimSpec :: ShowDimSpec dimspec => Proxy dimspec -> String
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

instance ShowDimSpec dims => Show (Dim dims) where
  show (Dim d) = (show d ++ showDimSpec (Proxy :: Proxy dims))