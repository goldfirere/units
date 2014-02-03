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

zero :: Num a => a
zero = 0

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
                
                
  unionWithAdd :: (Eq a, Num b) => [(a,b)] -> [(a,b)] -> [(a,b)]
  unionWithAdd m1 m2 = unionWithAdd_aux 
    (uniq (keys m1 ++ keys m2)) m1 m2
     
  unionWithAdd_aux :: (Eq a, Num b) => [a] -> [(a,b)] -> [(a,b)] -> [(a,b)]
  unionWithAdd_aux [] _ _ = []
  unionWithAdd_aux (x:xs) m1 m2 = (x, maybeAdd (lookup x m1)  (lookup x m2)) : unionWithAdd_aux xs m1 m2

  maybeAdd :: Num a => Maybe a -> Maybe a ->  a 
  maybeAdd (Just y1) (Just y2) = y1 + y2
  maybeAdd (Just y1) Nothing  = y1
  maybeAdd Nothing  (Just y2) = y2
  maybeAdd Nothing  Nothing  = zero

             
 |] )