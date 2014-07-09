{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies #-}

module Tests.Compile.TH where

import Data.Metrology.TH
import Data.Metrology.Poly
import qualified Data.Metrology as Mono

$(declareDimension "Length")
$(declareCanonicalUnit "Meter" [t| Length |] (Just "m"))
$(declareDerivedUnit "Foot" [t| Meter |] 0.3048 Nothing)

type MyLCSU = MkLCSU '[(Length, Meter)]

len1 :: MkQu_DLN Length MyLCSU Double
len1 = 5 % Meter

len2 :: MkQu_DLN Length MyLCSU Double
len2 = 10 % Foot

$(declareMonoUnit "Second" (Just "s"))
type Time = MkQu_D Second

time :: Time
time = 10 Mono.% Second
