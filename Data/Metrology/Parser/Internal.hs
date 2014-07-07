{- units Package
   Copyright (c) 2014 Richard Eisenberg

   This file defines a parser for unit expressions.
-}

{-# LANGUAGE LambdaCase, TemplateHaskell, NoMonomorphismRestriction,
             FlexibleContexts, GADTs, RankNTypes, DataKinds #-}

module Data.Metrology.Parser.Internal (
  parseUnitExp, parseUnitType,
  Goal(..),
  PrefixTable, UnitTable, SymbolTable(..), mkSymbolTable,
  ParserEnv(..),

  -- only for testing purposes:
  lex, unitStringParser
  ) where

import Prelude hiding ( lex, div )

import Text.Parsec         hiding ( tab )
import Text.Parsec.String
import Text.Parsec.Pos
import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Control.Monad.Reader
import Control.Arrow       hiding ( app)
import Data.Maybe

import Data.Metrology    hiding (Number(..))
import qualified Data.Metrology

import Language.Haskell.TH hiding ( Pred )

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
lexer = do
  spaces
  choice
    [ do eof <?> ""
         return []
    , do tok <- lexer1
         spaces
         toks <- lexer
         return (tok : toks)
    ]

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
-- Goal & parser environment
----------------------------------------------------------------------

data Goal g where
  Exp  :: Goal Exp
  Type :: Goal Type

data ParserEnv g = ParserEnv { goal   :: Goal g
                             , symTab :: SymbolTable }

-- either AppE or AppT, as appropriate
app :: MonadReader (ParserEnv g) m => m g -> m g -> m g
app ml mr = do
  l <- ml
  r <- mr
  g <- asks goal
  case g of
    Exp  -> return $ l `AppE` r
    Type -> return $ l `AppT` r

prefixOp, mulOp, divOp, powOp :: Goal g -> g

prefixOp Exp  = ConE '(:@)
prefixOp Type = ConT ''(:@)

mulOp Exp  = ConE '(:*)
mulOp Type = ConT ''(:*)

divOp Exp  = ConE '(:/)
divOp Type = ConT ''(:/)

powOp Exp  = ConE '(:^)
powOp Type = ConT ''(:^)

useOp :: MonadReader (ParserEnv g) m => (Goal g -> g) -> m g
useOp fn = asks (fn . goal)

appOp :: MonadReader (ParserEnv g) m => (Goal g -> g) -> m g -> m g -> m g
appOp op ml mr = app (app (useOp op) ml) mr

appPrefix, pow :: MonadReader (ParserEnv g) m => m g -> m g -> m g
appPrefix = appOp prefixOp
pow = appOp powOp

mul, div :: Goal g -> g -> g -> g
mul g@Exp  l r = mulOp g `AppE` l `AppE` r
mul g@Type l r = mulOp g `AppT` l `AppT` r

div g@Exp  l r = divOp g `AppE` l `AppE` r
div g@Type l r = divOp g `AppT` l `AppT` r


-- make an expression for a singleton Z
mkSingZExp :: Integer -> Exp
mkSingZExp n
  | n < 0     = VarE 'sPred `AppE` mkSingZExp (n + 1)
  | n > 0     = VarE 'sSucc `AppE` mkSingZExp (n - 1)
  | otherwise = VarE 'sZero

mkSingZType :: Integer -> Type
mkSingZType n
  | n < 0     = ConT ''Pred `AppT` mkSingZType (n + 1)
  | n > 0     = ConT ''Succ `AppT` mkSingZType (n - 1)
  | otherwise = ConT 'Zero   -- single quote, because Zero is a *data* con

mkZ :: MonadReader (ParserEnv g) m => Integer -> m g
mkZ n = do
  g <- asks goal
  case g of
    Exp  -> return $ mkSingZExp  n
    Type -> return $ mkSingZType n

ofType :: Name -> Exp
ofType n = (VarE 'undefined) `SigE` (ConT n)

use :: MonadReader (ParserEnv g) m => Name -> m g
use n = do
  g <- asks goal
  case g of
    Exp  -> return $ ofType n
    Type -> return $ ConT n

dimensionless :: MonadReader (ParserEnv g) m => m g
dimensionless = do
  g <- asks goal
  case g of
    Exp  -> return $ ConE 'Data.Metrology.Number
    Type -> return $ ConT ''Data.Metrology.Number

----------------------------------------------------------------------
-- Unit string parser
----------------------------------------------------------------------

-- We assume that no symbol table is inherently ambiguous!

type UnitStringParser g a = ParsecT String () (Reader (ParserEnv g)) a

-- parses just a unit (no prefix)
justUnitP :: UnitStringParser g Name
justUnitP = do
  full_string <- getInput
  units <- asks (unitTable . symTab)
  case Map.lookup full_string units of
    Nothing -> fail (full_string ++ " does not match any known unit")
    Just u  -> return u

-- parses a unit and prefix, failing in the case of ambiguity
prefixUnitP :: UnitStringParser g g
prefixUnitP = do
  prefixTab <- asks (prefixTable . symTab)
  let assocs = Map.assocs prefixTab  -- these are in the right order
  results <- catMaybes `liftM` mapM (experiment . parse_one) assocs
  full_string <- getInput
  case results of
    [] -> fail $ "No known interpretation for " ++ full_string
    [(pre_name, unit_name)] ->
      use pre_name `appPrefix` use unit_name
    lots -> fail $ "Multiple possible interpretations for " ++ full_string ++ ":\n" ++
                   (concatMap (\(pre_name, unit_name) ->
                                 "  " ++ show pre_name ++
                                 " :@ " ++ show unit_name ++ "\n") lots)
  where
    parse_one :: (String, Name) -> UnitStringParser g (Name, Name)
    parse_one (pre, name) = do
      void $ string pre
      unit_name <- justUnitP
      return (name, unit_name)

-- parse a unit string
unitStringParser :: UnitStringParser g g
unitStringParser = try (use =<< justUnitP) <|> prefixUnitP

----------------------------------------------------------------------
-- Unit expression parser
----------------------------------------------------------------------

type UnitParser g a = ParsecT [Token] () (Reader (ParserEnv g)) a

-- move a source position past a token
updatePosToken :: SourcePos -> Token -> [Token] -> SourcePos
updatePosToken pos (Unit unit_str) _ = updatePosString pos unit_str
updatePosToken pos (Number i) _      = updatePosString pos (show i)
updatePosToken pos (Op _) _          = incSourceColumn pos 1

-- parse a Token
uToken :: (Token -> Maybe a) -> UnitParser g a
uToken = tokenPrim show updatePosToken

-- consume an lparen
lparenP :: UnitParser g ()
lparenP = uToken $ \case
  Op OpenP -> Just ()
  _        -> Nothing

-- consume an rparen
rparenP :: UnitParser g ()
rparenP = uToken $ \case
  Op CloseP -> Just ()
  _         -> Nothing

-- parse a unit string
unitStringP :: String -> UnitParser g g
unitStringP str = do
  symbolTable <- ask
  case flip runReader symbolTable $ runParserT unitStringParser () "" str of
    Left err -> fail (show err)
    Right e  -> return e

-- parse a number, possibly negated and nested in parens
numP :: UnitParser g Integer
numP =
  do lparenP
     n <- numP
     rparenP
     return n
  <|>
  do uToken $ \case
       Op Neg -> Just ()
       _      -> Nothing
     negate `liftM` numP
  <|>
  do uToken $ \case
       Number i -> Just i
       _        -> Nothing

-- parse an exponentiation, like "^2"
powP :: UnitParser g (UnitParser g g -> UnitParser g g)
powP = option id $ do
  uToken $ \case
    Op Pow -> Just ()
    _      -> Nothing
  n <- numP
  return $ \base -> base `pow` mkZ n

-- parse a unit, possibly with an exponent
unitP :: UnitParser g g
unitP =
  do n <- numP
     case n of
       1 -> dimensionless
       _ -> unexpected $ "number " ++ show n
  <|>
  do unit_str <- uToken $ \case
       Unit unit_str -> Just unit_str
       _             -> Nothing
     maybe_pow <- powP
     maybe_pow $ unitStringP unit_str

-- parse a "unit factor": either a juxtaposed sequence of units
-- or a paranthesized unit exp.
unitFactorP :: UnitParser g g
unitFactorP =
  do lparenP
     unitExp <- parser
     rparenP
     return unitExp
  <|>
  do unitExps <- many1 unitP
     g <- asks goal
     return $ foldl1 (mul g) unitExps

-- parse * or /
opP :: UnitParser g (g -> g -> g)
opP = do
  g <- asks goal
  uToken $ \case
    Op Mult -> Just (mul g)
    Op Div  -> Just (div g)
    _       -> Nothing

-- parse a whole unit expression
parser :: UnitParser g g
parser = do
  d'less <- dimensionless
  chainl unitFactorP opP d'less

parseUnit :: Goal g -> SymbolTable -> String -> Either String g
parseUnit g tab s = left show $ do
  toks <- lex s
  flip runReader (ParserEnv g tab) $ runParserT (consumeAll parser) () "" toks

-- top-level interface
parseUnitExp :: SymbolTable -> String -> Either String Exp
parseUnitExp = parseUnit Exp

parseUnitType :: SymbolTable -> String -> Either String Type
parseUnitType = parseUnit Type
