{- units Package
   Copyright (c) 2014 Richard Eisenberg

   This file defines a parser for unit expressions.
-}

{-# LANGUAGE LambdaCase, TemplateHaskell, NoMonomorphismRestriction,
             FlexibleContexts #-}

module Data.Metrology.Parser (
  parseUnit,
  PrefixTable, UnitTable, SymbolTable(..), mkSymbolTable,

  -- only for testing purposes:
  lex, unitStringParser                                           
  ) where

import Prelude hiding ( lex )

import Text.Parsec         hiding ( tab )
import Text.Parsec.String
import Text.Parsec.Pos
import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Control.Monad.Reader
import Control.Arrow
import Data.Maybe

import Data.Metrology    hiding (Number(..))

import Language.Haskell.TH

----------------------------------------------------------------------
-- Basic combinators
----------------------------------------------------------------------

-- copied from GHC
partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

----------------------------------------------------------------------
-- Extra parser combinators
----------------------------------------------------------------------

-- | @experiment p@ runs @p@. If @p@ succeeds, @experiment@ returns the
-- result of running @p@. If @p@ fails, then @experiment@ returns @Nothing@.
-- In either case, no input is consumed and @experiment@ never fails.
experiment :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
experiment = lookAhead . optionMaybe . try

consumeAll :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m a
consumeAll p = do
  result <- p
  eof
  return result

nochar :: Stream s m Char => Char -> ParsecT s u m ()
nochar = void . char

----------------------------------------------------------------------
-- Datatypes
----------------------------------------------------------------------

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

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

type Lexer = Parser

unitL :: Lexer Token
unitL = Unit `fmap` (many1 letter)

opL :: Lexer Token
opL = fmap Op $
      do { nochar '-'; return Neg    }
  <|> do { nochar '*'; return Mult   }
  <|> do { nochar '/'; return Div    }
  <|> do { nochar '^'; return Pow    }
  <|> do { nochar '('; return OpenP  }
  <|> do { nochar ')'; return CloseP }

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

----------------------------------------------------------------------
-- Symbol tables
----------------------------------------------------------------------

type PrefixTable = Map.Map String Name
type UnitTable = Map.Map String Name
data SymbolTable = SymbolTable { prefixTable :: PrefixTable
                               , unitTable   :: UnitTable }
  deriving Show

-- build a Map from a list, checking for ambiguity
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

-- from a prefix table and a unit table, build a full, unambiguous symbol table
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

----------------------------------------------------------------------
-- Unit string parser
----------------------------------------------------------------------

-- We assume that no symbol table is inherently ambiguous!

type UnitStringParser = ParsecT String () (Reader SymbolTable)

ofType :: Name -> Exp
ofType n = (VarE 'undefined) `SigE` (ConT n)

-- parses just a unit (no prefix)
justUnitP :: UnitStringParser Name
justUnitP = do
  full_string <- getInput
  units <- asks unitTable
  case Map.lookup full_string units of
    Nothing -> fail (full_string ++ " does not match any known unit")
    Just u  -> return u

-- parses a unit and prefix, failing in the case of ambiguity
prefixUnitP :: UnitStringParser Exp
prefixUnitP = do
  prefixTab <- asks prefixTable
  let assocs = Map.assocs prefixTab  -- these are in the right order
  results <- catMaybes `liftM` mapM (experiment . parse_one) assocs
  full_string <- getInput
  case results of
    [] -> fail $ "No known interpretation for " ++ full_string
    [(pre_name, unit_name)] ->
      return $ ConE '(:@) `AppE` ofType pre_name `AppE` ofType unit_name
    lots -> fail $ "Multiple possible interpretations for " ++ full_string ++ ":\n" ++
                   (concatMap (\(pre_name, unit_name) ->
                                 "  " ++ show pre_name ++
                                 " :@ " ++ show unit_name ++ "\n") lots)
  where
    parse_one :: (String, Name) -> UnitStringParser (Name, Name)
    parse_one (pre, name) = do
      void $ string pre
      unit_name <- justUnitP
      return (name, unit_name)

-- parse a unit string
unitStringParser :: UnitStringParser Exp
unitStringParser = try (ofType `liftM` justUnitP) <|> prefixUnitP

----------------------------------------------------------------------
-- Unit expression parser
----------------------------------------------------------------------

type UnitParser = ParsecT [Token] () (Reader SymbolTable)

-- move a source position past a token
updatePosToken :: SourcePos -> Token -> [Token] -> SourcePos
updatePosToken pos (Unit unit_str) _ = updatePosString pos unit_str
updatePosToken pos (Number i) _      = updatePosString pos (show i)
updatePosToken pos (Op _) _          = incSourceColumn pos 1

-- parse a Token
uToken :: (Token -> Maybe a) -> UnitParser a
uToken = tokenPrim show updatePosToken

-- consume an lparen
lparenP :: UnitParser ()
lparenP = uToken $ \case
  Op OpenP -> Just ()
  _        -> Nothing

-- consume an rparen
rparenP :: UnitParser ()
rparenP = uToken $ \case
  Op CloseP -> Just ()
  _         -> Nothing

-- parse a unit string
unitStringP :: String -> UnitParser Exp
unitStringP str = do
  symbolTable <- ask
  case flip runReader symbolTable $ runParserT unitStringParser () "" str of
    Left err -> fail (show err)
    Right e  -> return e

-- make an expression for a singleton Z
mkSingZExp :: Integer -> Exp
mkSingZExp n
  | n < 0     = VarE 'sPred `AppE` mkSingZExp (n + 1)
  | n > 0     = VarE 'sSucc `AppE` mkSingZExp (n - 1)
  | otherwise = VarE 'sZero

-- parse a number, possibly negated and nested in parens
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

-- parse an exponentiation, like "^2"
powP :: UnitParser (Exp -> Exp)
powP = option id $ do
  uToken $ \case
    Op Pow -> Just ()
    _      -> Nothing
  n <- numP
  return $ \base -> ConE '(:^) `AppE` base `AppE` n

-- parse a unit, possibly with an exponent
unitP :: UnitParser Exp
unitP = do
  unit_str <- uToken $ \case
    Unit unit_str -> Just unit_str
    _             -> Nothing
  result <- unitStringP unit_str
  maybe_pow <- powP
  return $ maybe_pow result

-- parse a "unit factor": either a juxtaposed sequence of units
-- or a paranthesized unit exp.
unitFactorP :: UnitParser Exp
unitFactorP =
  do lparenP
     unitExp <- parser
     rparenP
     return unitExp
  <|>
  do unitExps <- many1 unitP
     return $ foldl1 (\l r -> (ConE '(:*)) `AppE` l `AppE` r) unitExps

-- parse * or /
opP :: UnitParser (Exp -> Exp -> Exp)
opP = do
  op <- uToken $ \case
    Op Mult -> Just '(:*)
    Op Div  -> Just '(:/)
    _       -> Nothing
  return $ \l r -> ConE op `AppE` l `AppE` r

-- parse a whole unit expression
parser :: UnitParser Exp
parser = do
  result <- chainl1 unitFactorP opP
  return result

-- top-level interface
parseUnit :: SymbolTable -> String -> Either String Exp
parseUnit tab s = left show $ do
  toks <- lex s
  flip runReader tab $ runParserT (consumeAll parser) () "" toks

