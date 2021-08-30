{-# LANGUAGE TypeFamilies, TypeOperators, PatternSynonyms, TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.SI
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports unit definitions according to the SI system of units.
-- The definitions were taken from here: <http://www.bipm.org/en/si/>.
--
-- Some additional units were added based on
-- <http://www.bipm.org/en/si/si_brochure/chapter4/table6.html this link>:
-- "Non-SI units accepted for use with the SI,
-- and units based on fundamental constants".
--
-- There is one deviation from the definitions at that site: To work better
-- with prefixes, the unit of mass is 'Gram'.
--
-- This module exports both American spellings and British spellings of
-- units, using pattern synonyms to get the British spellings of data
-- constructors.
-----------------------------------------------------------------------------

module Data.Units.SI where

import Data.Metrology.Poly
import Data.Metrology.TH
import Data.Dimensions.SI
import Data.Units.SI.Prefixes ( Kilo, Centi )
import Data.Constants.Math (piR)

import Language.Haskell.TH ( Name )

declareCanonicalUnit "Meter"   [t| Length |]                         (Just "m")

type Metre = Meter

pattern Metre :: Metre
pattern Metre = Meter

declareCanonicalUnit "Gram"    [t| Mass |]                           (Just "g")

type Gramme = Gram

pattern Gramme :: Gramme
pattern Gramme = Gram

declareCanonicalUnit "Second"  [t| Time |]                           (Just "s")

-- | Derived SI unit
declareDerivedUnit "Minute"    [t| Second |]                    60   (Just "min")

-- | Derived SI unit
declareDerivedUnit "Hour"      [t| Minute |]                    60   (Just "h")

declareCanonicalUnit "Ampere"  [t| Current |]                        (Just "A")
declareCanonicalUnit "Kelvin"  [t| Temperature |]                    (Just "K")
declareCanonicalUnit "Mole"    [t| AmountOfSubstance |]              (Just "mol")
declareCanonicalUnit "Candela" [t| LuminousIntensity |]              (Just "cd")

-- | The two angular dimensions that must be fundamental dimensions.
declareCanonicalUnit "Radian"     [t| PlaneAngle |]                  (Just "rad")
declareCanonicalUnit "Steradian"  [t| SolidAngle |]                  (Just "sr")

declareDerivedUnit "Hertz"     [t| Number :/ Second |]          1    (Just "Hz")

-- | This is not in the SI standard, but is used widely.
declareDerivedUnit "Liter"     [t| (Centi :@ Meter) :^ Three |] 1000 (Just "L")

type Litre = Liter

pattern Litre :: Litre
pattern Litre = Liter

declareDerivedUnit "Newton"    [t| Gram :* Meter :/ (Second :^ Two) |]  1000  (Just "N")
declareDerivedUnit "Pascal"    [t| Newton :/ (Meter :^ Two) |]          1     (Just "Pa")
declareDerivedUnit "Joule"     [t| Newton :* Meter |]                   1     (Just "J")
declareDerivedUnit "Watt"      [t| Joule :/ Second |]                   1     (Just "W")
declareDerivedUnit "Coulomb"   [t| Ampere :* Second |]                  1     (Just "C")
declareDerivedUnit "Volt"      [t| Watt :/ Ampere |]                    1     (Just "V")
declareDerivedUnit "Farad"     [t| Coulomb :/ Volt |]                   1     (Just "F")
declareDerivedUnit "Ohm"       [t| Volt :/ Ampere |]                    1     (Just "Ω")
declareDerivedUnit "Siemens"   [t| Ampere :/ Volt |]                    1     (Just "S")
declareDerivedUnit "Weber"     [t| Volt :* Second |]                    1     (Just "Wb")
declareDerivedUnit "Tesla"     [t| Weber :/ (Meter :^ Two) |]           1     (Just "T")
declareDerivedUnit "Henry"     [t| Weber :/ Ampere |]                   1     (Just "H")
declareDerivedUnit "Lumen"     [t| Candela |]                           1     (Just "lm")
declareDerivedUnit "Lux"       [t| Lumen :/ (Meter :^ Two) |]           1     (Just "lx")
declareDerivedUnit "Becquerel" [t| Number :/ Second |]                  1     (Just "Bq")
declareDerivedUnit "Gray"      [t| (Meter :^ Two) :/ (Second :^ Two) |] 1     (Just "Gy")
declareDerivedUnit "Sievert"   [t| (Meter :^ Two) :/ (Second :^ Two) |] 1     (Just "Sv")
declareDerivedUnit "Katal"     [t| Mole :/ Second |]                    1     (Just "kat")

-- Non-SI units accepted for use with SI 
-- (https://en.wikipedia.org/wiki/International_System_of_Units#Non-SI_units_accepted_for_use_with_SI)
declareDerivedUnit "Degree"       [t| Radian |]       (piR / 180)           (Just "deg")
-- This should be printed as /'/ but Text.Parse.Units.mkSymbolTable says that's illegal.
declareDerivedUnit "Arcminute"    [t| Degree |]       (1 / 60)              (Just "arcminute")
-- This should be printed as /"/ but Text.Parse.Units.mkSymbolTable says that's illegal.
declareDerivedUnit "Arcsecond"    [t| Arcminute |]    (1 / 60)              (Just "arcsecond")

-- | Derived SI unit
declareDerivedUnit "Hectare"   [t| Meter :^ Two |]                      10000 (Just "ha")

-- | Derived SI unit
declareDerivedUnit "Ton"       [t| Kilo :@ Gram |]                      1000  (Just "t")

type Tonne = Ton
pattern Tonne :: Tonne
pattern Tonne = Ton

-- | A list of the names of all unit types. Useful with
-- 'Data.Metrology.Parser.makeQuasiQuoter'.
siUnits :: [Name]
siUnits =
  [ ''Meter, ''Gram, ''Second, ''Minute, ''Hour, ''Ampere
  , ''Kelvin, ''Mole, ''Candela, ''Hertz, ''Liter, ''Newton, ''Pascal
  , ''Joule, ''Watt, ''Coulomb, ''Volt, ''Farad, ''Ohm, ''Siemens
  , ''Weber, ''Tesla, ''Henry, ''Lumen, ''Radian, ''Lux, ''Becquerel, ''Gray
  , ''Sievert, ''Katal, ''Degree, ''Arcminute
  , ''Arcsecond,''Hectare, ''Ton
  ]
