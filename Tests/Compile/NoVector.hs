module Tests.Compile.NoVector where

import Data.Metrology.NoVector
import Data.Metrology.SI

x = 5 % Meter
y = 2 % Second
vel = x |/| y
