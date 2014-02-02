{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, FlexibleContexts,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators, UndecidableInstances #-}

module Data.Quantity.Map where

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.List
import Data.Singletons.TH
import Data.Quantity.Zahl
import Prelude hiding (lookup)


$( promote [d|
  
  delete :: Eq a => a -> [a] -> [a]
  delete x [] = []
  delete x (y:ys) =
    if x==y then delete x ys else y:delete x ys     

  uniq :: Eq a => [a] -> [a]
  uniq [] = []
  uniq (x:xs) = x:uniq (delete x xs)
    

  keys :: [(a,b)] -> [a]
  keys [] = []
  keys ((x,y):xys) = x : keys xys

  elems :: [(a,b)] -> [b]
  elems [] = []
  elems ((x,y):xys) = y : elems xys



 |] )

$( promote [d|

  lookup :: Eq a => a -> [(a,b)] -> Maybe b
  lookup x [] = Nothing
  lookup x ((x1,y1): xys) = 
    if x==x1 then Just y1 else lookup x xys
                
                
  addMap :: (Eq a, Eq b, Num b) => [(a,b)] -> [(a,b)] -> [(a,b)]
  addMap m1 m2 = deleteZeroElems 
    (addMap_aux (uniq (keys m1 ++ keys m2)) m1 m2)
     
  addMap_aux :: (Eq a, Num b) => [a] -> [(a,b)] -> [(a,b)] -> [(a,b)]
  addMap_aux [] _ _ = []
  addMap_aux (x:xs) m1 m2 = (x, maybeAdd (lookup x m1)  (lookup x m2)) : addMap_aux xs m1 m2

  maybeAdd :: Num b => Maybe b -> Maybe b ->  b
  maybeAdd (Just y1) (Just y2) = y1 + y2
  maybeAdd (Just y1) Nothing  = y1
  maybeAdd Nothing  (Just y2) = y2
  maybeAdd Nothing  Nothing  = zero

  deleteZeroElems :: (Eq b, Num b) => [(a,b)] -> [(a,b)]
  deleteZeroElems [] = []
  deleteZeroElems ((x,y) : xys) =
    if y==zero then deleteZeroElems xys else (x,y) : deleteZeroElems xys

  eqMap :: (Eq a, Eq b, Num b) => [(a,b)] -> [(a,b)] -> Bool
  eqMap m1 m2 = 
    (eqMap_aux (uniq (keys m1 ++ keys m2)) m1 m2)

  eqMap_aux :: (Eq a, Eq b, Num b) => [a] -> [(a,b)] -> [(a,b)] -> Bool
  eqMap_aux [] _ _ = True
  eqMap_aux (x:xs) m1 m2 = maybeEq (lookup x m1) (lookup x m2) && eqMap_aux xs m1 m2

  maybeEq :: (Eq b, Num b) => Maybe b -> Maybe b ->  Bool
  maybeEq (Just y1) (Just y2) = y1 == y2
  maybeEq (Just y1) Nothing  = y1 == zero
  maybeEq Nothing  (Just y2) = y2 == zero
  maybeEq Nothing  Nothing  = True

 |] )
