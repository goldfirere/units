{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators #-}

import Data.Singletons.TH
import Unsafe.Coerce


$(singletons [d|

  data Nat = Zero | Succ Nat
    deriving Eq


  plus :: Nat -> Nat -> Nat
  plus Zero     n = n
  plus (Succ m) n = Succ (plus m n)

 |] )


$( promote [d| 

  headdrop :: [a] -> [a]
  headdrop [] = []
  headdrop (x:xs) = xs

 |] )


main :: IO ()
main = return ()