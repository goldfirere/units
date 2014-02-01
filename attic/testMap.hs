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

type LengthDim = '[ '( BaseDim Length , Posi 1) ] 
type SpeedDim = '[ '( BaseDim Length , Posi 1) ,  '( BaseDim Time , Nega 1) ] 
type TimeDim = '[ '( BaseDim Time , Posi 1) ]  



main :: IO ()
main = do
  printBool $ fromSing $ 
    (sing :: Sing ((Lookup (BaseDim Length) SpeedDim) :== ('Just (Posi 1))))  
