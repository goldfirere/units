{- Define test quasiquoters
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell, CPP #-}

module Tests.Compile.UnitParser.Quoters where

import Data.Metrology.Parser
import Data.Metrology.SI

$(makeQuasiQuoter "ms" [''Milli, ''Kilo] [''Meter, ''Second])

$(makeQuasiQuoter "si" siUnits siPrefixes)


#if __GLASGOW_HASKELL__ >= 709
$(do units <- allUnits
     prefixes <- allPrefixes
     makeQuasiQuoter "unit" units prefixes)
#endif
