{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Metrology
import Data.Metrology.Z
import Data.Metrology.SI.MonoTypes()
import Data.Metrology.SI.PolyTypes
import Data.Metrology.SI.Prefixes
import Data.Metrology.SI.Units
import Data.Metrology.SI.Poly (SI)
import qualified Data.Metrology.SI.Dims as D
import Data.Metrology.Show
import Data.Metrology.Unsafe -- forgive me!

type Six      = S Five
type Seven    = S Six
type Eight    = S Seven
type Nine     = S Eight
type Ten      = S Nine
type Eleven   = S Ten
type Twelve   = S Eleven
type Thirteen = S Twelve


pSix     = SS pFive
pSeven   = SS pSix     
pEight   = SS pSeven   
pNine    = SS pEight   
pTen     = SS pNine    
pEleven  = SS pTen     
pTwelve  = SS pEleven  
pThirteen= SS pTwelve  


data EV = EV
instance Show EV where show _ = "eV"
instance Unit EV where
  type BaseUnit EV = Joule 
  conversionRatio _ = 1.60217657e-19

data ProtonMass = ProtonMass
instance Show ProtonMass where show _ = "m_p"
instance Unit ProtonMass where
  type BaseUnit ProtonMass = Kilo :@ Gram
  conversionRatio _ = 1.67262178e-27

data Å = Å
instance Show Å where show _ = "Å"
instance Unit Å where
  type BaseUnit Å = Meter
  conversionRatio _ = 1e-8


-- | chemist's unit
type CU = MkLCSU '[ (D.Length, Å)
                  , (D.Mass, ProtonMass)
                  , (D.Time, Pico :@ Second)]

protonMass :: Mass SI Float
protonMass =  1 % ProtonMass

sigmaAr :: Length SI Float
sigmaAr = 3.4e-8 % Meter

epsAr :: Energy SI Float
epsAr = 1.68e-21 % Joule

ljForce :: Length SI Float -> Force SI Float
ljForce r = redim $ 24 *| epsAr |*| sigmaAr|^ pSix |/| r |^ pSeven
                |-| 48 *| epsAr |*| sigmaAr|^ pTwelve |/| r |^ pThirteen


ljForceP :: Energy l Float -> Length l Float -> Length l Float -> Force l Float
ljForceP eps sigma r 
  = redim $ 24 *| eps |*| sigma|^ pSix |/| r |^ pSeven
        |-| 48 *| eps |*| sigma|^ pTwelve |/| r |^ pThirteen


main :: IO ()
main = do
  putStrLn $ concat
     [ "A chemist said his favorite system of unit (CU) consists of "
     , "an Ångstrom, a proton mass, and a picosecond. They are "
     , ((Qu 1) :: Length CU Float) `showIn` Meter   , ", "
     , ((Qu 1) :: Mass CU Float) `showIn` kilo Gram , ", and "
     , ((Qu 1) :: Time CU Float) `showIn` Second    , ", respectively."
     ]
  

  putStrLn "He insists that it's better to do chemistry in CU than SI."
  putStrLn "For example, the attractive force between two Argon atom at the distance of 4Å is:"
  putStrLn $ ljForce (4 % Å) `showIn` (Newton)
  putStrLn "Oops, it is:"
  putStrLn $ (ljForceP (convert epsAr) (convert sigmaAr) (4 % Å) :: Force SI Float) `showIn` (Newton)
  putStrLn "I can't do it in SI! On the other hand in CU we can:"
  putStrLn $ (ljForceP (convert epsAr) (convert sigmaAr) (4 % Å) :: Force CU Float) `showIn` (Newton)


  -- how would you type it polymorphically (not using the default LCSU)?
  let ans :: (ConvertibleLCSUs_D D.Length SI l, ConvertibleLCSUs_D D.Energy SI l)=> Force l Float
      ans = ljForceP (convert epsAr) (convert sigmaAr) (4 % Å)

  putStrLn "We compare again:"  
  putStrLn $ (ans :: Force SI Float) `showIn` (Newton)
  putStrLn $ (ans :: Force CU Float) `showIn` (Newton)



{--- output ---
$ runhaskell LennardJones.hs
A chemist said his favorite system of unit (CU) consists of an Ångstrom, a proton mass, and a picosecond. They are 1.0e-8 m, 1.6726218e-27 kg, and 1.0e-12 s, respectively.
He insists that it's better to do chemistry in CU than SI.
For example, the attractive force between two Argon atom at the distance of 4Å is:
NaN N
Oops, it is:
NaN N
I can't do it in SI! On the other hand in CU we can:
9.3407324e-14 N
We compare again:
NaN N
9.3407324e-14 N

-}