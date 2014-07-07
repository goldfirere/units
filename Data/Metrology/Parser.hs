{-# LANGUAGE TemplateHaskell, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Parser
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports functions allowing users to create their own unit
-- quasiquoters to make for compact unit expressions.
--
-- A typical use case is this:
--
-- > $(makeQuasiQuoter "unit" [''Kilo, ''Milli] [''Meter, ''Second])
--
-- and then, /in a separate module/ (due to GHC's staging constraints)
--
-- > x = 3 % [unit| m/s^2 ]
--
-- The unit expressions can refer to the prefixes and units specified in
-- the call to 'makeQuasiQuoter'. The spellings of the prefixes and units
-- are taken from their @Show@ instances.
--
-- The syntax for these expressions is like
-- F#'s. There are four arithmetic operators (@*@, @/@, @^@, and juxtaposition).
-- Exponentiation binds the tightest, and it allows an integer to its right
-- (possibly with minus signs and parentheses). Next tightest is juxtaposition,
-- which indicates multiplication. Because juxtaposition binds tighter than division,
-- the expressions @m/s^2@ and @m/s s@ are equivalent. Multiplication and
-- division bind the loosest and are left-associative, meaning that @m/s*s@
-- is equivalent to @(m/s)*s@, probably not what you meant. Parentheses in
-- unit expressions are allowed, of course.
--
-- Within a unit string (that is, a unit with an optional prefix), there may
-- be ambiguity. If a unit string can be interpreted as a unit without a
-- prefix, that parsing is preferred. Thus, @min@ would be minutes, not
-- milli-inches (assuming appropriate prefixes and units available.) There still
-- may be ambiguity between unit strings, even interpreting the string as a prefix
-- and a base unit. If a unit string is amiguous in this way, it is rejected.
-- For example, if we have prefixes @da@ and @d@ and units @m@ and @am@, then
-- @dam@ is ambiguous like this.
-----------------------------------------------------------------------------

module Data.Metrology.Parser (
  makeQuasiQuoter, allUnits, allPrefixes
  ) where

import Prelude hiding ( exp )

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Desugar.Lift  ()  -- get the Lift Name instance
import Data.Maybe
import Control.Monad

import Data.Metrology.Parser.Internal
import Data.Metrology
import Data.Metrology.TH

emptyQQ :: QuasiQuoter
emptyQQ = QuasiQuoter { quoteExp = \_ -> fail "No quasi-quoter for expressions"
                      , quotePat = \_ -> fail "No quasi-quoter for patterns"
                      , quoteType = \_ -> fail "No quasi-quoter for types"
                      , quoteDec = \_ -> fail "No quasi-quoter for declarations" }

errorQQ :: String -> QuasiQuoter
errorQQ msg = QuasiQuoter { quoteExp = \_ -> fail msg
                          , quotePat = \_ -> fail msg
                          , quoteType = \_ -> fail msg
                          , quoteDec = \_ -> fail msg }

-- | @makeQuasiQuoter "qq" prefixes units@ makes a quasi-quoter named @qq@
-- that considers the prefixes and units provided. These are provided via
-- names of the /type/ constructors, /not/ the data constructors. See the
-- module documentation for more info and an example.
makeQuasiQuoter :: String -> [Name] -> [Name] -> Q [Dec]
makeQuasiQuoter qq_name_str prefix_names unit_names = do
  mapM_ checkIsType prefix_names
  mapM_ checkIsType unit_names
  qq <- [| case $sym_tab of
            Left err -> errorQQ err
            Right computed_sym_tab ->
              emptyQQ { quoteExp = \unit_exp ->
                         case parseUnitExp computed_sym_tab unit_exp of
                           Left err2 -> fail err2
                           Right exp -> return exp
                      , quoteType = \unit_exp ->
                         case parseUnitType computed_sym_tab unit_exp of
                           Left err2 -> fail err2
                           Right typ -> return typ
                      } |]
  return [ SigD qq_name (ConT ''QuasiQuoter)
         , ValD (VarP qq_name) (NormalB qq) []]
  where
    qq_name = mkName qq_name_str
    
    mk_pair :: Name -> Q Exp   -- Exp is of type (String, Name)
    mk_pair n = [| (show (undefined :: $( return $ ConT n )), n) |]

    sym_tab :: Q Exp           -- Exp is of type (Either String SymbolTable)
    sym_tab = do
      prefix_pairs <- mapM mk_pair prefix_names
      unit_pairs   <- mapM mk_pair unit_names
      [| mkSymbolTable $( return $ ListE prefix_pairs ) $( return $ ListE unit_pairs ) |]

getInstanceNames :: Name -> Q [Name]
getInstanceNames class_name = do
  ClassI _ insts <- reify class_name
  m_names <- forM insts $ \inst ->
    case inst of
      InstanceD _ ((ConT class_name') `AppT` (ConT unit_name)) []
        |  class_name == class_name'
        -> do show_insts <- reifyInstances ''Show [ConT unit_name]
              case show_insts of
                [_show_inst] -> return $ Just unit_name
                _            -> return Nothing
      _ -> return Nothing
  return $ catMaybes m_names

#if __GLASGOW_HASKELL__ < 709
{-# WARNING allUnits, allPrefixes "Retrieving the list of all units and prefixes in scope does not work under GHC 7.8.*. Please upgrade GHC to use these functions." #-}
#endif

-- | Gets a list of the names of all units with @Show@ instances in scope.
-- Example usage:
--
-- > $( do units <- allUnits
-- >       makeQuasiQuoter "unit" [] units )
--
allUnits :: Q [Name]
allUnits = getInstanceNames ''Unit

-- | Gets a list of the names of all unit prefixes with @Show@ instances in
-- scope. Example usage:
--
-- > $( do units    <- allUnits
-- >       prefixes <- allPrefixes
-- >       makeQuasiQuoter "unit" prefixes units )
--
allPrefixes :: Q [Name]
allPrefixes = getInstanceNames ''UnitPrefix
