{-# LANGUAGE TypeOperators, TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.SI.Prefixes
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines prefixes from the SI standard at <http://www.bipm.org/en/si/>
-----------------------------------------------------------------------------

module Data.Metrology.SI.Prefixes where

import Language.Haskell.TH ( Name )
import Data.Metrology

-- | 10^1
data Deca = Deca
instance UnitPrefix Deca where
  multiplier _ = 1e1
instance Show Deca where
  show _ = "da"

deca :: unit -> Deca :@ unit
deca = (Deca :@)

-- | 10^2
data Hecto = Hecto
instance UnitPrefix Hecto where
  multiplier _ = 1e2
instance Show Hecto where
  show _ = "h"

hecto :: unit -> Hecto :@ unit
hecto = (Hecto :@)

-- | 10^3
data Kilo = Kilo
instance UnitPrefix Kilo where
  multiplier _ = 1e3
instance Show Kilo where
  show _ = "k"

kilo :: unit -> Kilo :@ unit
kilo = (Kilo :@)

-- | 10^6
data Mega = Mega
instance UnitPrefix Mega where
  multiplier _ = 1e6
instance Show Mega where
  show _ = "M"

mega :: unit -> Mega :@ unit
mega = (Mega :@)

-- | 10^9
data Giga = Giga
instance UnitPrefix Giga where
  multiplier _ = 1e9
instance Show Giga where
  show _ = "G"

giga :: unit -> Giga :@ unit
giga = (Giga :@)

-- | 10^12
data Tera = Tera
instance UnitPrefix Tera where
  multiplier _ = 1e12
instance Show Tera where
  show _ = "T"

tera :: unit -> Tera :@ unit
tera = (Tera :@)

-- | 10^15
data Peta = Peta
instance UnitPrefix Peta where
  multiplier _ = 1e15
instance Show Peta where
  show _ = "P"

peta :: unit -> Peta :@ unit
peta = (Peta :@)

-- | 10^18
data Exa = Exa
instance UnitPrefix Exa where
  multiplier _ = 1e18
instance Show Exa where
  show _ = "E"

exa :: unit -> Exa :@ unit
exa = (Exa :@)

-- | 10^21
data Zetta = Zetta
instance UnitPrefix Zetta where
  multiplier _ = 1e21
instance Show Zetta where
  show _ = "Z"

zetta :: unit -> Zetta :@ unit
zetta = (Zetta :@)

-- | 10^24
data Yotta = Yotta
instance UnitPrefix Yotta where
  multiplier _ = 1e24
instance Show Yotta where
  show _ = "Y"

yotta :: unit -> Yotta :@ unit
yotta = (Yotta :@)

-- | 10^-1
data Deci = Deci
instance UnitPrefix Deci where
  multiplier _ = 1e-1
instance Show Deci where
  show _ = "d"

deci :: unit -> Deci :@ unit
deci = (Deci :@)

-- | 10^-2
data Centi = Centi
instance UnitPrefix Centi where
  multiplier _ = 1e-2
instance Show Centi where
  show _ = "c"

centi :: unit -> Centi :@ unit
centi = (Centi :@)

-- | 10^-3
data Milli = Milli
instance UnitPrefix Milli where
  multiplier _ = 1e-3
instance Show Milli where
  show _ = "m"

milli :: unit -> Milli :@ unit
milli = (Milli :@)

-- | 10^-6
data Micro = Micro
instance UnitPrefix Micro where
  multiplier _ = 1e-6
instance Show Micro where
  show _ = "μ"

micro :: unit -> Micro :@ unit
micro = (Micro :@)

-- | 10^-9
data Nano = Nano
instance UnitPrefix Nano where
  multiplier _ = 1e-9
instance Show Nano where
  show _ = "n"

nano :: unit -> Nano :@ unit
nano = (Nano :@)

-- | 10^-12
data Pico = Pico
instance UnitPrefix Pico where
  multiplier _ = 1e-12
instance Show Pico where
  show _ = "p"

pico :: unit -> Pico :@ unit
pico = (Pico :@)

-- | 10^-15
data Femto = Femto
instance UnitPrefix Femto where
  multiplier _ = 1e-15
instance Show Femto where
  show _ = "f"

femto :: unit -> Femto :@ unit
femto = (Femto :@)

-- | 10^-18
data Atto = Atto
instance UnitPrefix Atto where
  multiplier _ = 1e-18
instance Show Atto where
  show _ = "a"

atto :: unit -> Atto :@ unit
atto = (Atto :@)

-- | 10^-21
data Zepto = Zepto
instance UnitPrefix Zepto where
  multiplier _ = 1e-21
instance Show Zepto where
  show _ = "z"

zepto :: unit -> Zepto :@ unit
zepto = (Zepto :@)

-- | 10^-24
data Yocto = Yocto
instance UnitPrefix Yocto where
  multiplier _ = 1e-24
instance Show Yocto where
  show _ = "y"

yocto :: unit -> Yocto :@ unit
yocto = (Yocto :@)

-- | A list of the names of all prefix types. Useful with
-- 'Data.Metrology.Parser.makeQuasiQuoter'.
siPrefixes :: [Name]
siPrefixes =
  [ ''Deca, ''Hecto, ''Kilo, ''Mega, ''Giga, ''Tera, ''Peta, ''Exa
  , ''Zetta, ''Yotta, ''Deci, ''Centi, ''Milli, ''Micro, ''Nano
  , ''Pico, ''Femto, ''Atto, ''Zepto, ''Yocto
  ]
