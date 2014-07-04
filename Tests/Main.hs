{- Test/Main.hs

   The units Package
   Copyright (c) 2014 Richard Eisenberg
   eir@cis.upenn.edu

   This is the main testing file for the units package.
-}

{-# LANGUAGE ImplicitParams #-}

module Tests.Main where

import qualified Tests.Compile.CGS               ()
import qualified Tests.Compile.EvalType          ()
import qualified Tests.Compile.Lcsu              ()
import qualified Tests.Compile.MetrologySynonyms ()
import qualified Tests.Compile.Physics           ()
import qualified Tests.Compile.Quantity          ()
import qualified Tests.Compile.Readme            ()
import qualified Tests.Compile.Simulator         ()
import qualified Tests.Compile.UnitParser        ()
import qualified Tests.Compile.Units             ()

import qualified Tests.Imperial
import qualified Tests.LennardJones
import qualified Tests.OffSystemAdd
import qualified Tests.OffSystemCSU
import qualified Tests.Parser
import qualified Tests.PhysicalConstants
import qualified Tests.Show
import qualified Tests.Travel

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  let ?epsilon = 0.0000001 in
  testGroup "Tests"
  [ Tests.Imperial.tests
  , Tests.LennardJones.tests
  , Tests.OffSystemAdd.tests
  , Tests.OffSystemCSU.tests
  , Tests.Parser.tests
  , Tests.PhysicalConstants.tests
  , Tests.Show.tests
  , Tests.Travel.tests
  ]
