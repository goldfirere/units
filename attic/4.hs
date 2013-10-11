{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE PolyKinds, KindSignatures, UndecidableInstances #-}
import GHC.TypeLits hiding ((+)(..), (-)(..))
import qualified GHC.TypeLits as Nat
import Data.Dimensions.TypePrelude  
  
import Data.Dimensions.Zahl
main :: IO ()
main = do
  print $ fromSing (sing :: Sing (((Posi 2) - (Posi 4))))
  print $ fromSing (sing :: Sing (((Posi 2) - (Nega 4))))
  print $ fromSing (sing :: Sing (((Nega 2) - (Posi 4))))
  print $ fromSing (sing :: Sing (((Nega 2) - (Nega 4))))
  print $ fromSing (sing :: Sing (((Posi 12) - (Posi 4))))
  print $ fromSing (sing :: Sing (((Posi 12) - (Nega 4))))
  print $ fromSing (sing :: Sing (((Nega 12) - (Posi 4))))
  print $ fromSing (sing :: Sing (((Nega 12) - (Nega 4))))
  
  
--  print $ fromSing (sing :: Sing (((Posi 0) - (Posi 4))))  
--  print $ fromSing (sing :: Sing (If (4 <=? 0) (Posi (0 Nat.- 4)) (Nega (4 Nat.- 0))))  
  
  print $ fromSing (sing :: Sing (If (4 <=? 1) (Posi (1 Nat.- 4)) (Nega (4 Nat.- 1))))    
  
  
--  print $ fromSing (sing :: Sing (Posi (0 Nat.- 4)))
  print $ fromSing (sing :: Sing (3 <=? 4))
  print $ fromSing (sing :: Sing (0 <=? 4))
  print $ fromSing (sing :: Sing (4 <=? 0))
