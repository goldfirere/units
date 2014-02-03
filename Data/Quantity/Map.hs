{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators, UndecidableInstances #-}

module Data.Quantity.Map where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH
import Data.Quantity.Zahl


$( singletons [d| 
   
   data Map a b = FromList [(a,b)]

  |] )


$( promote [d| 

   lookup :: Eq a => a -> Map a b -> Maybe b
   lookup x (FromList []) = Nothing
   lookup x (FromList ((yk,yv):ys)) = 
     if x==yk then Just yv else lookup x (FromList ys)

 |] )

