{-# LANGUAGE EmptyCase, TemplateHaskell, PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes, TypeOperators, UndecidableInstances #-}


import Data.Singletons
import Data.Singletons.Bool
import Data.Singletons.Tuple
import Data.Singletons.TH
import Data.Quantity.Dimension
import Data.Quantity.Map
import Data.Quantity.Zahl
import Unsafe.Coerce

printBool :: Bool -> IO ()
printBool = print

main :: IO ()
main = do
  printBool $ fromSing $  (sing :: Sing ('Nothing :== ('Just Int)))  