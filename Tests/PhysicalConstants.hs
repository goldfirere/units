{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, TypeFamilies,
             TypeOperators, ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.PhysicalConstants where

import Data.Metrology.Poly
import Data.Metrology.Show ()
import Data.Metrology.SI.Poly
import qualified Data.Dimensions.SI as D

import Tests.Compile.MetrologySynonyms

import Test.Tasty.HUnit
import Test.HUnit.Approx

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

type instance DefaultUnitOfDim D.Mass = Kilo :@ Gram
type instance DefaultUnitOfDim D.Time = Second
type instance DefaultUnitOfDim D.Length = Meter
type instance DefaultUnitOfDim D.Current = Ampere
type instance DefaultUnitOfDim D.Temperature = Kelvin

-- | A unicornhorn of honor (unicornhorn in short) is the historical
-- unit of length used in Kingdom of Fantasia. A unicornhorn was
-- defined as the length of the horn of the King's honored
-- unicorn. Unfortunately, king's men did not documented their
-- technology, so today there's no telling how long a unicornhorn was.

data UnicornHorn = UnicornHorn
instance Unit UnicornHorn where
  type BaseUnit UnicornHorn = Canonical
  type DimOfUnit UnicornHorn = D.Length
instance Show UnicornHorn where
  show _ = "uhoh"

type MkQu_UL unit lcsu = MkQu_ULN unit lcsu Double

type KingdomUnit = MkLCSU '[ (D.Length, UnicornHorn) ]

solarMass :: DefaultConvertibleLCSU_D D.Mass l => Mass l Double
solarMass = constant $ 1.98892e33 % Gram

earthMass :: DefaultConvertibleLCSU_D D.Mass l => Mass l Double
earthMass = constant $ 5.9742e24 % Gram

electronMass :: DefaultConvertibleLCSU_D D.Mass l => Mass l Double
electronMass = constant $ 9.10938291e-28 % Gram

protonMass :: DefaultConvertibleLCSU_D D.Mass l => Mass l Double
protonMass = constant $ 1.67262178e-24 % Gram

speedOfLight :: DefaultConvertibleLCSU_D D.Velocity l => Velocity l Double
speedOfLight =  constant $ 299792458 % ((Second :^ sMOne) :* Meter)

elementaryCharge :: DefaultConvertibleLCSU_D D.Charge l => Charge l Double
elementaryCharge = constant $ 1.60217657e-19 % Coulomb

gravitationalConstant :: DefaultConvertibleLCSU_U SIGCUnit l => MkQu_UL SIGCUnit l
gravitationalConstant = constant $ 6.67384e-11 % (undefined :: SIGCUnit)

kB :: DefaultConvertibleLCSU_U SIkBUnit l => MkQu_UL SIkBUnit l
kB = constant $ 1.3806488e-23 % (undefined :: SIkBUnit)

-- RAE: problem solved. :)
planckLength :: DefaultConvertibleLCSU_D D.Length l => Length l Double
planckLength = constant $ qSqrt (hbar |*| gravitationalConstant |/| (speedOfLight |^ sThree))

planckTime :: DefaultConvertibleLCSU_D D.Time l => Time l Double
planckTime = constant $ planckLength |/| speedOfLight

-- eps0
vacuumPermittivity :: CompatibleUnit l SIPermittivityUnit
  => MkQu_UL SIPermittivityUnit l
vacuumPermittivity =  
  (1 / (4 * pi * 1e-7 * 299792458**2)) % (undefined :: SIPermittivityUnit)
-- mu0                     
vacuumPermeability :: CompatibleUnit l SIPermeabilityUnit 
  => MkQu_UL SIPermeabilityUnit l
vacuumPermeability = (4 * pi * 1e-7) % (undefined :: SIPermeabilityUnit)

-- |Planck constant
planckConstant :: CompatibleUnit l JouleSecond 
  => MkQu_UL JouleSecond l
planckConstant =  (6.6260695729e-34) % (undefined :: JouleSecond)

-- |Reduced Planck constant
hbar ::  CompatibleUnit l JouleSecond   => MkQu_UL JouleSecond l
hbar = (1 / 2 / pi) *| planckConstant

tests =
  let ?epsilon = 1e-40 in
  testCase "PhysicalConstants" ((planckLength :: Length SI Double) # Meter @?~ 1.616199e-35)

main :: IO ()
main = do
  putStrLn "typechecks!"
  print (planckLength :: Length SI Double)
--  print (planckLength :: Length KingdomUnit Double)
-- last line does not typecheck -- good!

{- output --

typechecks!
1.616199256057012e-35 m

-}


