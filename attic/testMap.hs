{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes, TypeOperators, UndecidableInstances #-}


import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH
import Data.Quantity.Dimension
import Data.Quantity.Dimension.SI
import Data.Quantity.Map
import Data.Quantity.Zahl
import Unsafe.Coerce

printBool :: Bool -> IO ()
printBool = print

type LengthQu = '[ '( Qu Length , Posi 1) ] 
type SpeedQu = '[ '( Qu Length , Posi 1) ,  '( Qu Time , Nega 1) ] 
type TimeQu = '[ '( Qu Time , Posi 1) ]  



main :: IO ()
main = do
  printBool $ fromSing $ 
    (sing :: Sing ((Lookup (Qu Length) SpeedDim) :== ('Just (Posi 1))))  
