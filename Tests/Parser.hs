{- Test the unit parser
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell #-}

module Tests.Parser where

import Prelude hiding ( lex, exp )

import Data.Metrology.Parser.Internal
import Data.Metrology.SI

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Text.Parsec
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

pprintUnqualified :: (Ppr a, Data a) => a -> String
pprintUnqualified = pprint . everywhere (mkT (mkName . nameBase))

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

lexTest :: String -> String
lexTest s =
  case lex s of
    Left _     -> "error"
    Right toks -> show toks

lexTestCases :: [(String, String)]
lexTestCases = [ ( "m", "[m]" )
               , ( "", "[]" )
               , ( "m s", "[m,s]" )
               , ( "   m   s ", "[m,s]" )
               , ( "m   ", "[m]" )
               , ( "   m", "[m]" )
               , ( "( m  /s", "[(,m,/,s]" )
               , ( "!", "error" )
               ]

lexTests :: TestTree
lexTests = testGroup "Lexer" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ lexTest str @?= out) lexTestCases

----------------------------------------------------------------------
-- Unit strings
----------------------------------------------------------------------

unitStringTestCases :: [(String, String)]
unitStringTestCases = [ ("m", "undefined :: Meter")
                      , ("s", "undefined :: Second")
                      , ("min", "undefined :: Minute")
                      , ("km", "(:@) (undefined :: Kilo) (undefined :: Meter)")
                      , ("mm", "(:@) (undefined :: Milli) (undefined :: Meter)")
                      , ("kmin", "(:@) (undefined :: Kilo) (undefined :: Minute)")
                      , ("dam", "error")   -- ambiguous!
                      , ("damin", "(:@) (undefined :: Deca) (undefined :: Minute)")
                      , ("ms", "(:@) (undefined :: Milli) (undefined :: Second)")
                      , ("mmin", "(:@) (undefined :: Milli) (undefined :: Minute)")
                      , ("mmm", "error")
                      , ("mmmin", "error")
                      , ("sm", "error")
                      , ("", "error")
                      , ("dak", "error")
                      , ("das", "(:@) (undefined :: Deca) (undefined :: Second)")
                      , ("ds", "(:@) (undefined :: Deci) (undefined :: Second)")
                      , ("daam", "(:@) (undefined :: Deca) (undefined :: Ampere)")
                      , ("kam", "(:@) (undefined :: Kilo) (undefined :: Ampere)")
                      , ("dm", "(:@) (undefined :: Deci) (undefined :: Meter)")
                      ]

parseUnitStringTest :: String -> String
parseUnitStringTest s =
  case flip runReader (ParserEnv Exp testSymbolTable) $ runParserT unitStringParser () "" s of
    Left _ -> "error"
    Right exp -> pprintUnqualified exp

unitStringTests :: TestTree
unitStringTests = testGroup "UnitStrings" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitStringTest str @?= out)
    unitStringTestCases

----------------------------------------------------------------------
-- Symbol tables
----------------------------------------------------------------------

mkSymbolTableTests :: TestTree
mkSymbolTableTests = testGroup "mkSymbolTable"
  [ testCase "Unambiguous1" (Map.keys (prefixTable testSymbolTable) @?= ["d","da","k","m"])
  , testCase "Unambiguous2" (Map.keys (unitTable testSymbolTable) @?= ["am","m","min","s"])
  , testCase "AmbigPrefix" (leftOnly (mkSymbolTable [("a",''Milli),("a",''Centi)] []) @?= Just "The label `a' is assigned to the following meanings:\n[Data.Metrology.SI.Prefixes.Milli,Data.Metrology.SI.Prefixes.Centi]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "AmbigUnit" (leftOnly (mkSymbolTable [] [("m",''Meter),("m",''Minute)]) @?= Just "The label `m' is assigned to the following meanings:\n[Data.Metrology.SI.Units.Meter,Data.Metrology.SI.Units.Minute]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "MultiAmbig" (leftOnly (mkSymbolTable [("a",''Milli),("b",''Centi),("b",''Deci),("b",''Kilo),("c",''Atto),("c",''Deca)] [("m",''Meter),("m",''Minute),("s",''Second)]) @?= Just "The label `b' is assigned to the following meanings:\n[Data.Metrology.SI.Prefixes.Centi,Data.Metrology.SI.Prefixes.Deci,Data.Metrology.SI.Prefixes.Kilo]\nThe label `c' is assigned to the following meanings:\n[Data.Metrology.SI.Prefixes.Atto,Data.Metrology.SI.Prefixes.Deca]\nThis is ambiguous. Please fix before building a unit parser.")
                                                                                                ]

testSymbolTable :: SymbolTable
Right testSymbolTable =
   mkSymbolTable [ ("k", ''Kilo)
                 , ("da", ''Deca)
                 , ("m", ''Milli)
                 , ("d", ''Deci) ]
                 [ ("m", ''Meter)
                 , ("s", ''Second)
                 , ("min", ''Minute)
                 , ("am", ''Ampere) ]

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
  [ lexTests
  , mkSymbolTableTests
  , unitStringTests
  , parseUnitTests
  , parseUnitTestsT
  ]
