{-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators, UndecidableInstances #-}

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.TH
import Unsafe.Coerce


$(singletons [d|

  data Nat = Zero | Succ Nat
    deriving Eq

  |] )

$( promote [d| 

  plus :: Nat -> Nat -> Nat
  plus Zero     n = n
  plus (Succ m) n = Succ (plus m n)

 |] )


$( promoteOnly [d| 

   isElem :: Eq a => a -> [a] -> Bool
   isElem x [] = False
   isElem x (y:ys) = (x==y) || isElem x ys
 |] )

type ListBBI = '[Bool, Bool, Int]



printBool :: Bool -> IO ()
printBool = print
-- ans =



main :: IO ()
main = do
  printBool $ fromSing $  (sing :: Sing ('Nothing :== ('Just Int)))
  printBool $ fromSing $  (sing :: Sing (IsElem 'Nothing  '[ 'Just Int] ))
  printBool $ fromSing $  (sing :: Sing (IsElem 'Nothing  '[ 'Just Int, 'Nothing] ))
