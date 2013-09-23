{- Data/Dimensions/Map.hs

   The units Package
   Copyright (c) 2013 Richard Eisenberg 
   eir@cis.upenn.edu

   This file defines the type-level Map and operations over them
-}

{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

module Data.Dimensions.Map where

type Map a b = '[ '(a, b)]