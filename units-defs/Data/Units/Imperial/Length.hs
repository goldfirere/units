{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Units.Imperial.Length
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Due to the <https://en.wikipedia.org/wiki/International_yard_and_pound
-- International yard and pound agreement of 1959>, it so happens that
-- Imperial and US customary units of length have the same base (the
-- international yard at 0.9144 meters). However, if subdivisions are
-- generally the same, Imperial units feature multiples of the yard that
-- US customary ones generally don't (e.g. the Imperial furlong is defined
-- by reference to the international yard, not the US survey one).
--
-- Where possible, reference have been made to UK legislation. However,
-- Wikipedia's page is /much/ better organized than any government
-- resource immediately available.
--
-- The UK legislation used as references are as follows:
-- <http://www.legislation.gov.uk/ukpga/1985/72/enacted>
-- <http://www.legislation.gov.uk/uksi/1994/2867/schedule/part/VI/made>
-- <http://www.legislation.gov.uk/uksi/1995/1804/schedule/made>
-----------------------------------------------------------------------------

module Data.Units.Imperial.Length where

import Data.Metrology.TH

import qualified Data.Units.US.Misc as US

import Language.Haskell.TH

declareDerivedUnit "Thou"    [t| US.Mil  |] 1  (Just "th")
declareDerivedUnit "Inch"    [t| US.Inch |] 1  (Just "in")
declareDerivedUnit "Foot"    [t| US.Foot |] 1  (Just "ft")
declareDerivedUnit "Yard"    [t| US.Yard |] 1  (Just "yd")
declareDerivedUnit "Chain"   [t| Yard    |] 22 (Just "ch")
declareDerivedUnit "Furlong" [t| Chain   |] 10 (Just "fur")
declareDerivedUnit "Mile"    [t| US.Mile |] 1  (Just "mi")
declareDerivedUnit "League"  [t| Mile    |] 3  (Just "lea")

declareDerivedUnit "Hand" [t| Inch |] 4 (Just "hand")

-- | Common lengths units: 'Foot', 'Inch', 'Yard', and 'Mile'
commonLengths :: [Name]
commonLengths = [ ''Foot, ''Inch, ''Yard, ''Mile ]

-- | Other lengths units: 'Thou', 'Hand', 'Chain', 'Furlong' and 'League'
otherLengths :: [Name]
otherLengths = [ ''Thou, ''Hand, ''Chain, ''Furlong, ''League ]
