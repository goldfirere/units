{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators, UndecidableInstances #-}

module Data.Quantity.Map where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH
import Data.Quantity.Zahl

$( promote [d| 

   lookup :: Eq a => a -> [(a,b)] -> Maybe b
   lookup x [] = Nothing
   lookup x ((yk,yv):ys) = 
     if x==yk then Just yv else lookup x ys

 |] )

