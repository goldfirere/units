{- units Package
   Copyright (c) 2014 Richard Eisenberg

   This file defines a parser for unit expressions.
-}

{-# LANGUAGE LambdaCase, TemplateHaskell, NoMonomorphismRestriction #-}

module Parser where

import Prelude hiding ( lex )

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos
import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Control.Monad.Reader
import Data.Generics
import Control.Arrow
import Data.Maybe

import Data.Metrology    hiding (Number(..))
import Data.Metrology.Z
import Data.Metrology.SI

import Language.Haskell.TH

import Test.Tasty
import Test.Tasty.HUnit

data Op = Neg | Mult | Div | Pow | OpenP | CloseP

instance Show Op where
  show Neg    = "-"
  show Mult   = "*"
  show Div    = "/"
  show Pow    = "^"
  show OpenP  = "("
  show CloseP = ")"

data Token = Unit String
           | Number Integer
           | Op Op

instance Show Token where
  show (Unit s)   = s
  show (Number i) = show i
  show (Op op)    = show op

type Lexer = Parser

unitL :: Lexer Token
unitL = Unit `fmap` (many1 letter)

opL :: Lexer Token
opL = fmap Op $
      do { char '-'; return Neg    }
  <|> do { char '*'; return Mult   }
  <|> do { char '/'; return Div    }
  <|> do { char '^'; return Pow    }
  <|> do { char '('; return OpenP  }
  <|> do { char ')'; return CloseP }

numberL :: Lexer Token
numberL = (Number . read) `fmap` (many1 digit)

lexer1 :: Lexer Token
lexer1 = unitL <|> opL <|> numberL

lexer :: Lexer [Token]
lexer = do { eof <?> "" ; return [] } <|> do
  spaces
  tok <- lexer1
  spaces
  toks <- lexer
  return (tok : toks)

lex :: String -> Either ParseError [Token]
lex = parse lexer ""

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

type PrefixTable = Map.Map String Name
type UnitTable = Map.Map String Name
data SymbolTable = SymbolTable { prefixTable :: PrefixTable
                               , unitTable   :: UnitTable }
  deriving Show

type UnitStringParser = ParsecT String () (Reader SymbolTable)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
-- ^ Uses a function to determine which of two output lists an input element should join
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

leftOnly :: Either a b -> Maybe a
leftOnly (Left a) = Just a
leftOnly (Right _) = Nothing

unambFromList :: Ord a => [(a,b)] -> Either [(a,[b])] (Map.Map a b)
unambFromList list =
  let multimap      = MM.fromList list
      assocs        = MM.assocs multimap
      (errs, goods) = partitionWith (\(key, vals) ->
                                       case vals of
                                         [val] -> Right (key, val)
                                         _     -> Left (key, vals)) assocs
      result        = Map.fromList goods
  in
  if null errs then Right result else Left errs

mkSymbolTable :: [(String, Name)] -> [(String, Name)] -> Either String SymbolTable
mkSymbolTable prefixes units =
  let result = do
        prefixTab <- unambFromList prefixes
        unitTab   <- unambFromList units
        return $ SymbolTable { prefixTable = prefixTab, unitTable = unitTab }
  in left ((++ error_suffix) . concatMap mk_error_string) result
  where
    mk_error_string :: (String, [Name]) -> String
    mk_error_string (k, vs) =
      "The label `" ++ k ++ "' is assigned to the following meanings:\n" ++
      show vs ++ "\n"
    error_suffix = "This is ambiguous. Please fix before building a unit parser."

mkSymbolTableTests :: TestTree
mkSymbolTableTests = testGroup "mkSymbolTable"
  [ testCase "Unambiguous1" (Map.keys (prefixTable testSymbolTable) @?= ["d","da","k","m"])
  , testCase "Unambiguous2" (Map.keys (unitTable testSymbolTable) @?= ["am","m","min","s"])
  , testCase "AmbigPrefix" (leftOnly (mkSymbolTable [("a",'Milli),("a",'Centi)] []) @?= Just "The label `a' is assigned to the following meanings:\n[Data.Metrology.SI.Prefixes.Milli,Data.Metrology.SI.Prefixes.Centi]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "AmbigUnit" (leftOnly (mkSymbolTable [] [("m",'Meter),("m",'Minute)]) @?= Just "The label `m' is assigned to the following meanings:\n[Data.Metrology.SI.Units.Meter,Data.Metrology.SI.Units.Minute]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "MultiAmbig" (leftOnly (mkSymbolTable [("a",'Milli),("b",'Centi),("b",'Deci),("b",'Kilo),("c",'Atto),("c",'Deca)] [("m",'Meter),("m",'Minute),("s",'Second)]) @?= Just "The label `b' is assigned to the following meanings:\n[Data.Metrology.SI.Prefixes.Centi,Data.Metrology.SI.Prefixes.Deci,Data.Metrology.SI.Prefixes.Kilo]\nThe label `c' is assigned to the following meanings:\n[Data.Metrology.SI.Prefixes.Atto,Data.Metrology.SI.Prefixes.Deca]\nThis is ambiguous. Please fix before building a unit parser.")
                                                                                                ]

testSymbolTable :: SymbolTable
Right testSymbolTable =
   mkSymbolTable [ ("k", 'Kilo)
                 , ("da", 'Deca)
                 , ("m", 'Milli)
                 , ("d", 'Deci) ]
                 [ ("m", 'Meter)
                 , ("s", 'Second)
                 , ("min", 'Minute)
                 , ("am", 'Ampere) ]

justUnitP :: UnitStringParser Name
justUnitP = do
  full_string <- getInput
  units <- asks unitTable
  case Map.lookup full_string units of
    Nothing -> fail (full_string ++ " does not match any known unit")
    Just u  -> return u

pprintUnqualified :: (Ppr a, Data a) => a -> String
pprintUnqualified = pprint . everywhere (mkT (mkName . nameBase))

-- | @experiment p@ runs @p@. If @p@ succeeds, @experiment@ returns the
-- result of running @p@. If @p@ fails, then @experiment@ returns @Nothing@.
-- In either case, no input is consumed and @experiment@ never fails.
experiment :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
experiment = lookAhead . optionMaybe . try

prefixUnitP :: UnitStringParser Exp
prefixUnitP = do
  prefixTab <- asks prefixTable
  let assocs = Map.assocs prefixTab  -- these are in the right order
  results <- catMaybes `liftM` mapM (experiment . parse_one) assocs
  full_string <- getInput
  case results of
    [] -> fail $ "No known interpretation for " ++ full_string
    [(pre_name, unit_name)] ->
      return $ ConE '(:@) `AppE` ConE pre_name `AppE` ConE unit_name
    lots -> fail $ "Multiple possible interpretations for " ++ full_string ++ ":\n" ++
                   (concatMap (\(pre_name, unit_name) ->
                                 "  " ++ show pre_name ++
                                 " :@ " ++ show unit_name ++ "\n") lots)
  where
    parse_one :: (String, Name) -> UnitStringParser (Name, Name)
    parse_one (pre, name) = do
      string pre
      unit_name <- justUnitP
      return (name, unit_name)

unitStringParser :: UnitStringParser Exp
unitStringParser = try (ConE `liftM` justUnitP) <|> prefixUnitP

unitStringTestCases :: [(String, String)]
unitStringTestCases = [ ("m", "Meter")
                      , ("s", "Second")
                      , ("min", "Minute")
                      , ("km", "(:@) Kilo Meter")
                      , ("mm", "(:@) Milli Meter")
                      , ("kmin", "(:@) Kilo Minute")
                      , ("dam", "error")   -- ambiguous!
                      , ("damin", "(:@) Deca Minute")
                      , ("ms", "(:@) Milli Second")
                      , ("mmin", "(:@) Milli Minute")
                      , ("mmm", "error")
                      , ("mmmin", "error")
                      , ("sm", "error")
                      , ("", "error")
                      , ("dak", "error")
                      , ("das", "(:@) Deca Second")
                      , ("ds", "(:@) Deci Second")
                      , ("daam", "(:@) Deca Ampere")
                      , ("kam", "(:@) Kilo Ampere")
                      , ("dm", "(:@) Deci Meter")
                      ]

parseUnitStringTest :: String -> String
parseUnitStringTest s =
  case flip runReader testSymbolTable $ runParserT unitStringParser () "" s of
    Left _ -> "error"
    Right exp -> pprintUnqualified exp

unitStringTests :: TestTree
unitStringTests = testGroup "UnitStrings" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitStringTest str @?= out)
    unitStringTestCases


type UnitParser = ParsecT [Token] () (Reader SymbolTable)

updatePosToken :: SourcePos -> Token -> [Token] -> SourcePos
updatePosToken pos (Unit unit_str) _ = updatePosString pos unit_str
updatePosToken pos (Number i) _      = updatePosString pos (show i)
updatePosToken pos (Op _) _          = incSourceColumn pos 1

uToken :: (Token -> Maybe a) -> UnitParser a
uToken = tokenPrim show updatePosToken

unitStringP :: String -> UnitParser Exp
unitStringP str = do
  symbolTable <- ask
  case flip runReader symbolTable $ runParserT unitStringParser () "" str of
    Left err -> fail (show err)
    Right e  -> return e

unitP :: UnitParser Exp
unitP = do
  unit_str <- uToken $ \case
    Unit unit_str -> Just unit_str
    _             -> Nothing
  result <- unitStringP unit_str
  maybe_pow <- powP
  return $ maybe_pow result

lparenP :: UnitParser ()
lparenP = uToken $ \case
  Op OpenP -> Just ()
  _        -> Nothing

rparenP :: UnitParser ()
rparenP = uToken $ \case
  Op CloseP -> Just ()
  _         -> Nothing

numP :: UnitParser Exp
numP =
  do lparenP
     n <- numP
     rparenP
     return n
  <|>
  do uToken $ \case
       Op Neg -> Just ()
       _      -> Nothing
     n <- numP
     return $ (VarE 'sNegate) `AppE` n
  <|>
  do uToken $ \case
       Number i -> Just (mkSingZExp i)
       _        -> Nothing

mkSingZExp :: Integer -> Exp
mkSingZExp n
  | n < 0     = VarE 'sPred `AppE` mkSingZExp (n + 1)
  | n > 0     = VarE 'sSucc `AppE` mkSingZExp (n - 1)
  | otherwise = VarE 'sZero

powP :: UnitParser (Exp -> Exp)
powP = option id $ do
  uToken $ \case
    Op Pow -> Just ()
    _      -> Nothing
  n <- numP
  return $ \base -> ConE '(:^) `AppE` base `AppE` n

unitFactorP :: UnitParser Exp
unitFactorP =
  do lparenP
     unitExp <- parser
     rparenP
     return unitExp
  <|>
  do unitExps <- many1 unitP
     return $ foldl1 (\l r -> (ConE '(:*)) `AppE` l `AppE` r) unitExps

opP :: UnitParser (Exp -> Exp -> Exp)
opP = do
  op <- uToken $ \case
    Op Mult -> Just '(:*)
    Op Div  -> Just '(:/)
    _       -> Nothing
  return $ \l r -> ConE op `AppE` l `AppE` r

parser :: UnitParser Exp
parser = do
  result <- chainl1 unitFactorP opP
  return result

consumeAll :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
consumeAll p = do
  result <- p
  eof
  return result
  
parseUnit :: SymbolTable -> String -> Either ParseError Exp
parseUnit tab s = do
  toks <- lex s
  flip runReader tab $ runParserT (consumeAll parser) () "" toks

parseUnitTest :: String -> String
parseUnitTest s =
  case parseUnit testSymbolTable s of
    Left _    -> "error"
    Right exp -> pprintUnqualified exp

parseTestCases :: [(String, String)]
parseTestCases =
  [ ("m", "Meter")
  , ("s", "Second")
  , ("ms", "(:@) Milli Second")
  , ("mm", "(:@) Milli Meter")
  , ("mmm", "error")
  , ("km", "(:@) Kilo Meter")
  , ("m s", "(:*) Meter Second")
  , ("m/s", "(:/) Meter Second")
  , ("m/s^2", "(:/) Meter ((:^) Second (sSucc (sSucc sZero)))")
  , ("s/m m", "(:/) Second ((:*) Meter Meter)")
  , ("s s/m m", "(:/) ((:*) Second Second) ((:*) Meter Meter)")
  , ("s*s/m*m", "(:*) ((:/) ((:*) Second Second) Meter) Meter")
  , ("s*s/(m*m)", "(:/) ((:*) Second Second) ((:*) Meter Meter)")
  , ("m^-1", "(:^) Meter (sNegate (sSucc sZero))")
  , ("m^(-1)", "(:^) Meter (sNegate (sSucc sZero))")
  , ("m^(-(1))", "(:^) Meter (sNegate (sSucc sZero))")
  ]

parseUnitTests :: TestTree
parseUnitTests = testGroup "ParseUnit" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitTest str @?= out)
    parseTestCases

tests :: TestTree
tests = testGroup "Parser"
  [ lexTests
  , mkSymbolTableTests
  , unitStringTests
  , parseUnitTests
  ]
