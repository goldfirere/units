{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tests.Compile.MetrologySynonyms where

import           Data.Metrology
import           Data.Metrology.SI.Poly
import           Data.Metrology.Unsafe
import           Text.Printf

----------------------------------------------------------------
-- Pretty Printing Functions
----------------------------------------------------------------

ppValF :: PrintfArg x => String -> Qu d l x -> String
ppValF fmtStr (Qu x) = printf fmtStr x --  ++ showFactor (Proxy :: Proxy (LookupList dims lcsu)))

ppValE :: PrintfArg x => Int -> Qu d l x -> String
ppValE d (Qu x) = ret
  where
    fmtStr :: String
    fmtStr = printf "%%.%de" d
    
    protoStr :: String
    protoStr = printf fmtStr x

    (valPart,expPart) = break (=='e') protoStr
    
    ret = case expPart of
      "e0" -> valPart
      _ -> printf "%s \\times 10^{%s}" valPart (drop 1 expPart)



----------------------------------------------------------------
-- Nonweighted units
----------------------------------------------------------------

type PerSecond =  Number :/ Second
type PerCm3 =  Second :^ MThree
type PerCm2 =  Second :^ MTwo
type PerCm = (Centi :@ Meter) :^ MTwo 
type GHz =  (Giga :@ Hertz)

----------------------------------------------------------------
-- Weighted units
----------------------------------------------------------------

-- densities
type GramPerCm2 = Gram :* PerCm2
type GramPerCm3 = Gram :* PerCm3 

type JouleM3 = Joule :/ (Meter :^ Three)


-- see the table in http://en.wikipedia.org/wiki/Spectral_irradiance
type SpectralRadiance = Watt :/ (Meter :^ Two) :/ Hertz


-- | Spectral Flux Density
-- type SpectralFluxDensity = '[ '(Mass, POne), '(Time, NTwo)] 
-- |Unit of EDP
data Jansky = Jansky
instance Show Jansky where show _ = "Jy"

instance Unit Jansky where
  type BaseUnit Jansky = Joule :/ (Meter :^ Two) :* Second
  conversionRatio _ = 1e-26


-- energies
data ElectronVolt = ElectronVolt
instance Show ElectronVolt where show _ = "eV"
instance Unit ElectronVolt where
  type BaseUnit ElectronVolt = Joule 
  conversionRatio _ = 1.60217657e-19


type JouleSecond = Joule :* Second


-- velocities
type KmPerSec = (Kilo :@ Meter) :/ Second
type MPerSec = Meter :/ Second
type CmPerSec = (Centi :@ Meter) :/ Second

type Kg = Kilo :@ Gram

type GramPerMole = Gram :/ Mole


data AU = AU
instance Show AU where show _ = "au"
instance Unit AU where
  type BaseUnit AU = Meter
  conversionRatio _ = 149597870700 

data Parsec = Parsec
instance Show Parsec where show _ = "pc"
instance Unit Parsec where
  type BaseUnit Parsec = Meter
  conversionRatio _ = 3.08567758e16



-- squared velocities
type Cm2PerSec2 = CmPerSec :^ Two
type Meter2PerSec2 = MPerSec :^ Two
type Sec2PerMeter2 = MPerSec :^ MTwo

-- areas
type Meter2 = Meter :^ Two
type Cm2 = (Centi :@ Meter) :^ Two

type SIGCUnit =
  (Meter :^ Three) :* ((Kilo :@ Gram) :^ MOne) :* (Second :^ MTwo)
type SIkBUnit = Joule :/ Kelvin


----------------------------------------------------------------
-- Electric Units
----------------------------------------------------------------
type VoltPerCm = Volt :/ (Centi :@ Meter)
type KVPerCm = (Kilo :@ Volt) :/ (Centi :@ Meter)


type CoulombPerCm2 = Coulomb :/ Cm2

-- eps0
type SIPermittivityUnit = 
  ((Kilo :@ Gram) :^ MOne) :*
  (Meter :^ MThree) :*
  (Second :^ Four) :*
  (Ampere :^ Two)

-- mu0
type SIPermeabilityUnit = 
  ((Kilo :@ Gram) :^ One) :*
  (Meter :^ One) :*
  (Second :^ MTwo) :*
  (Ampere :^ MTwo)


-- dipole moment
data Debye = Debye
instance Unit Debye where
  type BaseUnit Debye = Coulomb :* Meter
  conversionRatio _ = 1e-21/299792458
  
  
