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
  (char 'm' >> return (ConE 'Meter)) <|>
  (char 's' >> return (ConE 'Second))

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
  unitStringP unit_str

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
parser = chainl1 unitFactorP opP
  
parseUnit :: String -> Either ParseError Exp
parseUnit s = do
  toks <- lex s
  parse parser "" toks
