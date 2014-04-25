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


-- This doesn't work; I accept that it does not work, because the
-- compiler needs to know how to stage the hbar, G and c into the
-- lcsu.
--
-- planckLength :: (Compatible l Meter) => Length l Double


-- This does work, but this approach does not scale. You'd need a
-- `Compatible` clause for every kind of units involved.
--
-- planckLength :: (Compatible l SIGCUnit, Compatible l JouleSecond, Compatible l MPerSec, Compatible l Meter) => Length l Double

-- By the way, the final (Compatible l Meter) can be omitted. The
-- following works.
--
-- planckLength :: (Compatible l SIGCUnit, Compatible l JouleSecond, Compatible l MPerSec) => Length l Double


-- I want something like this to work. In this way, we need at most
-- several constraints (the number is the number of the base
-- dimensions involved.) Sadly, our current `units` design is such
-- that all units of a dimension are not necessary convertible.  If
-- there is a way to specify all independent base units and make
-- planckLength work, I think that's also a scalable approach and is
-- fine.
--
planckLength :: (Compatible l Gram, Compatible l Meter, Compatible l Second) => Length l Double
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