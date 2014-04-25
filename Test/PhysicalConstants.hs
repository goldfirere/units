{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Main where

import Data.Metrology
import Data.Metrology.SI.Units
import Data.Metrology.SI.GenTypes
import qualified Data.Metrology.SI.Dims as D
import Data.Metrology.Poly

import MetrologySynonyms

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: CompatibleDim l D.Mass => Mass l Double
solarMass = constant $ 1.98892e33 % Gram

earthMass :: CompatibleDim l D.Mass => Mass l Double
earthMass = constant $ 5.9742e24 % Gram

electronMass :: CompatibleDim l D.Mass => Mass l Double
electronMass = constant $ 9.10938291e-28 % Gram

protonMass :: CompatibleDim l D.Mass => Mass l Double
protonMass = constant $ 1.67262178e-24 % Gram

speedOfLight :: CompatibleDim l D.Velocity => Velocity l Double
speedOfLight =  constant $ 299792458 % (Meter :/ Second)

elementaryCharge :: (CompatibleDim l D.Charge) => Charge l Double
elementaryCharge = constant $ 1.60217657e-19 % Coulomb

gravitationalConstant :: CompatibleUnit l SIGCUnit => QuOfUL SIGCUnit l
gravitationalConstant = constant $ 6.67384e-11 % (undefined :: SIGCUnit)

kB :: CompatibleUnit l SIkBUnit => QuOfUL SIkBUnit l
kB = constant $ 1.3806488e-23 % (undefined :: SIkBUnit)

-- RAE: problem solved. :)
planckLength :: CompatibleDim l D.Length => Length l Double
planckLength = constant $ qSqrt (hbar |*| gravitationalConstant |/| (speedOfLight |^ pThree))

-- eps0
vacuumPermittivity :: CompatibleUnit l SIPermittivityUnit
  => QuOfUL SIPermittivityUnit l
vacuumPermittivity =  
  (1 / (4 * pi * 1e-7 * 299792458**2)) % (undefined :: SIPermittivityUnit)
-- mu0                     
vacuumPermeability :: CompatibleUnit l SIPermeabilityUnit 
  => QuOfUL SIPermeabilityUnit l
vacuumPermeability = (4 * pi * 1e-7) % (undefined :: SIPermeabilityUnit)

-- |Planck constant
planckConstant :: CompatibleUnit l JouleSecond 
  => QuOfUL JouleSecond l
planckConstant =  (6.6260695729e-34) % (undefined :: JouleSecond)

-- |Reduced Planck constant
hbar ::  CompatibleUnit l JouleSecond   => QuOfUL JouleSecond l
hbar = (1 / 2 / pi) *| planckConstant

main :: IO ()
main = putStrLn "typechecks!"

