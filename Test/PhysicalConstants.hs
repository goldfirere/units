{-# LANGUAGE ConstraintKinds #-}
module Main where

import Data.Metrology
import Data.Metrology.SI.Units
import Data.Metrology.SI.GenTypes

import MetrologySynonyms

----------------------------------------------------------------
-- Mass units
----------------------------------------------------------------

solarMass :: Compatible l Gram => Mass l Double
solarMass = 1.98892e33 % Gram

earthMass :: Compatible l Gram => Mass l Double
earthMass = 5.9742e24 % Gram

electronMass :: Compatible l Gram => Mass l Double
electronMass = 9.10938291e-28 % Gram

protonMass :: Compatible l Gram => Mass l Double
protonMass = 1.67262178e-24 % Gram


speedOfLight :: Compatible l MPerSec => Velocity l Double
speedOfLight =  299792458 % (Meter :/ Second)


elementaryCharge :: (Compatible l Coulomb) => Charge l Double
elementaryCharge = 1.60217657e-19 % Coulomb


gravitationalConstant :: Compatible l SIGCUnit => QuOfUL SIGCUnit l
gravitationalConstant = 6.67384e-11 % (undefined :: SIGCUnit)

kB :: Compatible l SIkBUnit => QuOfUL SIkBUnit l
kB = 1.3806488e-23 % (undefined :: SIkBUnit)


planckLength :: (Compatible l SIGCUnit, Compatible l JouleSecond, Compatible l MPerSec, Compatible l Meter) => Length l Double
planckLength = qSqrt (hbar |*| gravitationalConstant |/| (speedOfLight |^ pThree))

-- eps0
vacuumPermittivity :: Compatible l SIPermittivityUnit
  => QuOfUL SIPermittivityUnit l
vacuumPermittivity =  
  (1 / (4 * pi * 1e-7 * 299792458**2)) % (undefined :: SIPermittivityUnit)
-- mu0                     
vacuumPermeability :: Compatible l SIPermeabilityUnit 
  => QuOfUL SIPermeabilityUnit l
vacuumPermeability = (4 * pi * 1e-7) % (undefined :: SIPermeabilityUnit)

-- |Planck constant
planckConstant :: Compatible l JouleSecond 
  => QuOfUL JouleSecond l
planckConstant =  (6.6260695729e-34) % (undefined :: JouleSecond)

-- |Reduced Planck constant
hbar ::  Compatible l JouleSecond   => QuOfUL JouleSecond l
hbar = (1 / 2 / pi) *| planckConstant

main :: IO ()
main = putStrLn "typechecks!"