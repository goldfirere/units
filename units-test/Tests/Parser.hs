{- Test the unit parser
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell #-}

module Tests.Parser where

import Prelude hiding ( lex, exp )

import Data.Metrology.Parser
import Data.Metrology.SI

import Language.Haskell.TH
import Data.Generics

import Test.Tasty
import Test.Tasty.HUnit

leftOnly :: Either a b -> Maybe a
leftOnly (Left a) = Just a
leftOnly (Right _) = Nothing

----------------------------------------------------------------------
-- TH functions
----------------------------------------------------------------------

stripModules :: Data a => a -> a
stripModules = everywhere (mkT (mkName . nameBase))

pprintUnqualified :: (Ppr a, Data a) => a -> String
pprintUnqualified = pprint . stripModules

testSymbolTable :: SymbolTable Name Name
Right testSymbolTable =
   mkSymbolTable (stripModules [ ("k", ''Kilo)
                               , ("da", ''Deca)
                               , ("m", ''Milli)
                               , ("d", ''Deci) ])
                 (stripModules [ ("m", ''Meter)
                               , ("s", ''Second)
                               , ("min", ''Minute)
                               , ("am", ''Ampere) ])

----------------------------------------------------------------------
-- Overall parser
----------------------------------------------------------------------

parseUnitTest :: String -> String
parseUnitTest s =
  case parseUnitExp testSymbolTable s of
    Left _    -> "error"
    Right exp -> pprintUnqualified exp

parseTestCases :: [(String, String)]
parseTestCases =
  [ ("m", "undefined :: Meter")
  , ("s", "undefined :: Second")
  , ("ms", "(:@) (undefined :: Milli) (undefined :: Second)")
  , ("mm", "(:@) (undefined :: Milli) (undefined :: Meter)")
  , ("mmm", "error")
  , ("km", "(:@) (undefined :: Kilo) (undefined :: Meter)")
  , ("m s", "(:*) (undefined :: Meter) (undefined :: Second)")
  , ("m/s", "(:/) (undefined :: Meter) (undefined :: Second)")
  , ("m/s^2", "(:/) (undefined :: Meter) ((:^) (undefined :: Second) (sSucc (sSucc sZero)))")
  , ("s/m m", "(:/) (undefined :: Second) ((:*) (undefined :: Meter) (undefined :: Meter))")
  , ("s s/m m", "(:/) ((:*) (undefined :: Second) (undefined :: Second)) ((:*) (undefined :: Meter) (undefined :: Meter))")
  , ("s*s/m*m", "(:*) ((:/) ((:*) (undefined :: Second) (undefined :: Second)) (undefined :: Meter)) (undefined :: Meter)")
  , ("s*s/(m*m)", "(:/) ((:*) (undefined :: Second) (undefined :: Second)) ((:*) (undefined :: Meter) (undefined :: Meter))")
  , ("m^-1", "(:^) (undefined :: Meter) (sPred sZero)")
  , ("m^(-1)", "(:^) (undefined :: Meter) (sPred sZero)")
  , ("m^(-(1))", "(:^) (undefined :: Meter) (sPred sZero)")
  , ("1", "Number")
  , ("1/s", "(:/) Number (undefined :: Second)")
  , ("m 1 m", "(:*) ((:*) (undefined :: Meter) Number) (undefined :: Meter)")
  , ("  ", "Number")
  , ("", "Number")
  ]

parseUnitTests :: TestTree
parseUnitTests = testGroup "ParseUnit" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitTest str @?= out)
    parseTestCases

parseUnitTestT :: String -> String
parseUnitTestT s =
  case parseUnitType testSymbolTable s of
    Left _    -> "error"
    Right exp -> pprintUnqualified exp

parseTestCasesT :: [(String, String)]
parseTestCasesT =
  [ ("m", "Meter")
  , ("s", "Second")
  , ("ms", ":@ Milli Second")
  , ("mm", ":@ Milli Meter")
  , ("mmm", "error")
  , ("km", ":@ Kilo Meter")
  , ("m s", ":* Meter Second")
  , ("m/s", ":/ Meter Second")
  , ("m/s^2", ":/ Meter (:^ Second (Succ (Succ Zero)))")
  , ("s/m m", ":/ Second (:* Meter Meter)")
  , ("s s/m m", ":/ (:* Second Second) (:* Meter Meter)")
  , ("s*s/m*m", ":* (:/ (:* Second Second) Meter) Meter")
  , ("s*s/(m*m)", ":/ (:* Second Second) (:* Meter Meter)")
  , ("m^-1", ":^ Meter (Pred Zero)")
  , ("m^(-1)", ":^ Meter (Pred Zero)")
  , ("m^(-(1))", ":^ Meter (Pred Zero)")
  , ("1", "Number")
  , ("1/s", ":/ Number Second")
  , ("m 1 m", ":* (:* Meter Number) Meter")
  , ("  ", "Number")
  , ("", "Number")
  ]

parseUnitTestsT :: TestTree
parseUnitTestsT = testGroup "ParseUnitType" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitTestT str @?= out)
    parseTestCasesT

----------------------------------------------------------------------
-- Conclusion
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Parser"
  [ parseUnitTests
  , parseUnitTestsT
  ]
