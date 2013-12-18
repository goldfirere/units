{- Data/Dimensions/SI.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg
   eir@cis.upenn.edu

   This module defines the units, types, and prefixes from the SI system,
   as put forth here: http://www.bipm.org/en/si/
-}

{-# LANGUAGE TypeFamilies, TypeOperators #-}

{-| This module exports unit, type, and prefix definitions according to the SI
    system of units. The definitions were taken from here:
    <http://www.bipm.org/en/si/>.

    There is one deviation from the definition at that site: To work better
    with prefixes, the unit of mass is 'Gram'.
-}

module Data.Dimensions.SI (
  module Data.Dimensions.SI.Units,
  module Data.Dimensions.SI.Types,
  module Data.Dimensions.SI.Prefixes
  ) where

import Data.Dimensions.SI.Units
import Data.Dimensions.SI.Types
import Data.Dimensions.SI.Prefixes

