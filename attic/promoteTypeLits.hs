{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
             TypeOperators, UndecidableInstances #-}

import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.TH
import Data.Quantity.Zahl
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
  printBool $ fromSing $  (sing :: Sing (IsElem (Posi 3)  '[ Posi 2, Posi 3, Posi 5] ))
  printBool $ fromSing $  (sing :: Sing (IsElem (Posi 3)  '[ Posi 2, Posi 4, Posi 5] ))
  printBool $ fromSing $  (sing :: Sing (IsElem (Nega 0)  '[ Posi 2, Posi 0, Posi 5] )) -- +0 and -0 are equal!
