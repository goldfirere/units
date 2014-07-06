{- Test units package quasiquoter mechanism
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE QuasiQuotes, CPP #-}

module Tests.Compile.UnitParser where

import Tests.Compile.UnitParser.Quoters ( ms )
import Data.Metrology.SI
import Data.Metrology

frac1 :: Count
frac1 =  0.78 % [ms|  |]

len1, len2 :: Length
len1 = 5 % [ms| m |]
len2 = redim $ 10 % [ms| s km / ms |]

vel1, vel2 :: Velocity
vel1 = 5 % [ms| m/s |]
vel2 = redim $ 10 % [ms| m s/s^2 |]

acc1, acc2 :: Acceleration
acc1 = 5 % [ms| m/s^2 |]
acc2 = redim $ 10 % [ms| m/s s |]

#if __GLASGOW_HASKELL >= 709
len3 :: Length
len3 = 15 % [unit| μm |]

freq :: Frequency
freq = 100 % [unit| MHz |]
#endif
