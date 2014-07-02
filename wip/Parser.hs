{- units Package
   Copyright (c) 2014 Richard Eisenberg

   This file defines a parser for unit expressions.
-}

{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Parser where

import Prelude hiding ( lex )

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos

import Data.Metrology    hiding (Number(..))
import Data.Metrology.Z
import Data.Metrology.SI

import Language.Haskell.TH

import Test.HUnit

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

unitL :: Parser Token
unitL = Unit `fmap` (many1 letter)

op :: Parser Token
op = fmap Op $
      do { char '-'; return Neg    }
  <|> do { char '*'; return Mult   }
  <|> do { char '/'; return Div    }
  <|> do { char '^'; return Pow    }
  <|> do { char '('; return OpenP  }
  <|> do { char ')'; return CloseP }

number :: Parser Token
number = (Number . read) `fmap` (many1 digit)

lexer1 :: Parser Token
lexer1 = unitL <|> op <|> number

lexer :: Parser [Token]
lexer = do { eof <?> "" ; return [] } <|> do
  spaces
  tok <- lexer1
  spaces
  toks <- lexer
  return (tok : toks)

lex :: String -> Either ParseError [Token]
lex = parse lexer ""

type TokParser = GenParser Token ()

updatePosToken :: SourcePos -> Token -> [Token] -> SourcePos
updatePosToken pos (Unit unit_str) _ = updatePosString pos unit_str
updatePosToken pos (Number i) _      = updatePosString pos (show i)
updatePosToken pos (Op _) _          = incSourceColumn pos 1

appPrefix :: Exp
appPrefix = ConE '(:@)

justUnitP :: Parser Exp
justUnitP =
  (char 'm' >> eof >> return (ConE 'Meter)) <|>
  (char 's' >> eof >> return (ConE 'Second))

prefixP :: Parser Exp
prefixP =
  (char 'k' >> return (ConE 'Kilo)) <|>
  (char 'm' >> return (ConE 'Milli))

unitStringP' :: Parser Exp
unitStringP' =
  try justUnitP <|> do
    prefixExp <- prefixP
    unitExp <- justUnitP
    return (appPrefix `AppE` prefixExp `AppE` unitExp)

unitStringP :: String -> TokParser Exp
unitStringP str =
  case parse unitStringP' "" str of
    Left err -> fail (show err)
    Right e  -> return e

uToken :: (Token -> Maybe a) -> TokParser a
uToken = tokenPrim show updatePosToken

unitP :: TokParser Exp
unitP = do
  unit_str <- uToken $ \case
    Unit unit_str -> Just unit_str
    _             -> Nothing
  result <- unitStringP unit_str
  maybe_pow <- powP
  return $ maybe_pow result

lparenP :: TokParser ()
lparenP = uToken $ \case
  Op OpenP -> Just ()
  _        -> Nothing

rparenP :: TokParser ()
rparenP = uToken $ \case
  Op CloseP -> Just ()
  _         -> Nothing

numP :: TokParser Exp
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

powP :: TokParser (Exp -> Exp)
powP = option id $ do
  uToken $ \case
    Op Pow -> Just ()
    _      -> Nothing
  n <- numP
  return $ \base -> ConE '(:^) `AppE` base `AppE` n

unitFactorP :: TokParser Exp
unitFactorP =
  do lparenP
     unitExp <- parser
     rparenP
     return unitExp
  <|>
  do unitExps <- many1 unitP
     return $ foldl1 (\l r -> (ConE '(:*)) `AppE` l `AppE` r) unitExps

opP :: TokParser (Exp -> Exp -> Exp)
opP = do
  op <- uToken $ \case
    Op Mult -> Just '(:*)
    Op Div  -> Just '(:/)
    _       -> Nothing
  return $ \l r -> ConE op `AppE` l `AppE` r

parser :: TokParser Exp
parser = do
  result <- chainl1 unitFactorP opP
  return result

consumeAll :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
consumeAll p = do
  result <- p
  eof
  return result
  
parseUnit :: String -> Either ParseError Exp
parseUnit s = do
  toks <- lex s
  parse (consumeAll parser) "" toks

parseUnitTest :: String -> String
parseUnitTest s =
  case parseUnit s of
    Left error -> show error
    Right exp  -> pprint exp

testCases :: [(String, String)]
testCases =
  [ ("m", "Data.Metrology.SI.Units.Meter")
  , ("s", "Data.Metrology.SI.Units.Second")
  , ("ms", "(Data.Metrology.Combinators.:@) Data.Metrology.SI.Prefixes.Milli Data.Metrology.SI.Units.Second")
  , ("mm", "(Data.Metrology.Combinators.:@) Data.Metrology.SI.Prefixes.Milli Data.Metrology.SI.Units.Meter")
  , ("mmm", "(line 1, column 4):\n(line 1, column 3):\nunexpected 'm'\nexpecting end of input")
  , ("km", "(Data.Metrology.Combinators.:@) Data.Metrology.SI.Prefixes.Kilo Data.Metrology.SI.Units.Meter")
  , ("m s", "(Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Meter Data.Metrology.SI.Units.Second")
  , ("m/s", "(Data.Metrology.Combinators.:/) Data.Metrology.SI.Units.Meter Data.Metrology.SI.Units.Second")
  , ("m/s^2", "(Data.Metrology.Combinators.:/) Data.Metrology.SI.Units.Meter ((Data.Metrology.Combinators.:^) Data.Metrology.SI.Units.Second (Data.Metrology.Z.sSucc (Data.Metrology.Z.sSucc Data.Metrology.Z.sZero)))")
  , ("s/m m", "(Data.Metrology.Combinators.:/) Data.Metrology.SI.Units.Second ((Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Meter Data.Metrology.SI.Units.Meter)")
  , ("s s/m m", "(Data.Metrology.Combinators.:/) ((Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Second Data.Metrology.SI.Units.Second) ((Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Meter Data.Metrology.SI.Units.Meter)")
  , ("s*s/m*m", "(Data.Metrology.Combinators.:*) ((Data.Metrology.Combinators.:/) ((Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Second Data.Metrology.SI.Units.Second) Data.Metrology.SI.Units.Meter) Data.Metrology.SI.Units.Meter")
  , ("s*s/(m*m)", "(Data.Metrology.Combinators.:/) ((Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Second Data.Metrology.SI.Units.Second) ((Data.Metrology.Combinators.:*) Data.Metrology.SI.Units.Meter Data.Metrology.SI.Units.Meter)")
  , ("m^-1", "(Data.Metrology.Combinators.:^) Data.Metrology.SI.Units.Meter (Data.Metrology.Z.sNegate (Data.Metrology.Z.sSucc Data.Metrology.Z.sZero))")
  , ("m^(-1)", "(Data.Metrology.Combinators.:^) Data.Metrology.SI.Units.Meter (Data.Metrology.Z.sNegate (Data.Metrology.Z.sSucc Data.Metrology.Z.sZero))")
  , ("m^(-(1))", "(Data.Metrology.Combinators.:^) Data.Metrology.SI.Units.Meter (Data.Metrology.Z.sNegate (Data.Metrology.Z.sSucc Data.Metrology.Z.sZero))")
  ]

tests :: Test
tests = TestList $ map (\(str, out) -> TestCase $ parseUnitTest str @?= out) testCases
