{- Test/Main.hs

   The units Package
   Copyright (c) 2014 Richard Eisenberg
   eir@cis.upenn.edu

   This is the main testing file for the units package.
-}

module Tests.Main where

import qualified Tests.Compile.CGS
import qualified Tests.Compile.Lcsu
{-
import qualified Test.Compile.MetrologySynonyms
import qualified Test.Compile.OffSystem
import qualified Test.Compile.OffSystemAdd
import qualified Test.Compile.OffSystemCSU
import qualified Test.Compile.PhysicalConstants
import qualified Test.Compile.Physics
import qualified Test.Compile.Readme
import qualified Test.Compile.Simulator
import qualified Test.Compile.Spec
import qualified Test.Compile.Travel
import qualified Test.Compile.Units
-}

import qualified Tests.Imperial
import qualified Tests.LennardJones


import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Tests.Imperial.tests
  , Tests.LennardJones.tests ]
