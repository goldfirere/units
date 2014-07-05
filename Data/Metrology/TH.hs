-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.TH
-- Copyright   :  (C) 2014 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports Template Haskell functions to make working with
-- @units@ a little more convenient.
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Data.Metrology.TH (
  evalType,
  declareDimension, declareCanonicalUnit, declareDerivedUnit, declareMonoUnit,

  -- for internal use only
  checkIsType                                    
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Desugar.Expand
import Language.Haskell.TH.Desugar.Lift ()   -- need Lift Rational

import Data.Metrology.Dimensions
import Data.Metrology.Units
import Data.Metrology.LCSU

-- | "Evaluates" a type as far as it can. This is useful, say, in instance
-- declarations:
--
-- > instance Show $(evalType [t| Length |]) where ...
--
-- Without the 'evalType', the instance declaration fails because @Length@
-- mentions type families, which can't be used in instance declarations.
--
-- This function is somewhat experimental, and will likely not work with
-- more polymorphic types. (If it doesn't work, not all of the type families
-- will be evaluated, and the instance declaration will fail. This function
-- should never cause /incorrect/ behavior.)
evalType :: Q Type -> Q Type
evalType qty = do
  ty <- qty
  dty <- dsType ty
  ex_dty <- expandType dty
  return $ sweeten ex_dty

-- Checks to make sure the given name names a /type/, not a /data constructor/.
-- Reports a compile-time error if the name is not a type.
checkIsType :: Name -> Q ()
checkIsType n = do
  info <- reify n
  case info of
    ClassOpI {} -> generic_error
    DataConI {} -> datacon_error
    VarI {} -> generic_error
    _ -> return ()
  where
    generic_error = reportError $ "The name " ++ show n ++ " does not describe a type.\n    A type is expected here."
    datacon_error = reportError $ "The name " ++ show n ++ " describes a data constructor.\n    Did you perhaps mean to say ''" ++ nameBase n ++ "? Note the two quotes."

-- | Declare a new dimension of the given name:
--
-- > $(declareDimension "Length")
--
-- produces
--
-- > data Length = Length
-- > instance Dimension Length
declareDimension :: String -> Q [Dec]
declareDimension str =
  return [ DataD [] name [] [NormalC name []] []
         , InstanceD [] (ConT ''Dimension `AppT` ConT name) [] ]
  where
    name = mkName str

-- | Conditionally generates a @Show@ instance
maybeMkShowInstance :: Name -> Maybe String -> Q [Dec]
maybeMkShowInstance name (Just abbrev) =
  [d| instance Show $(return $ ConT name) where { show _ = abbrev } |]
maybeMkShowInstance _ Nothing = return []

-- | @declareCanonicalUnit unit_name dim (Just abbrev)@ creates a new
-- canonical unit (that is, it is not defined in terms of other known units)
-- named @unit_name@, measuring dimension @dim@. (@dim@ must be the name of
-- the dimension /type/, not /data constructor/.) @abbrev@ will be the
-- abbreviation in the unit's @Show@ instance. If no abbraviation is supplied,
-- then no @Show@ instance will be generated.
--
-- Example usage:
--
-- > $(declareCanonicalUnit "Meter" ''Length (Just "m"))
declareCanonicalUnit :: String -> Name -> Maybe String -> Q [Dec]
declareCanonicalUnit unit_name_str dim m_abbrev = do
  checkIsType dim
  show_instance <- maybeMkShowInstance unit_name m_abbrev
  unit_instance <- [d| instance Unit $unit_type where
                         type BaseUnit $unit_type = Canonical
                         type DimOfUnit $unit_type = $dim_type |]
  return $ (DataD [] unit_name [] [NormalC unit_name []] [])
           : unit_instance ++ show_instance
  where
    unit_name = mkName unit_name_str
    unit_type = return $ ConT unit_name
    dim_type = return $ ConT dim

-- | @declareDerivedUnit unit_name base_unit_type ratio (Just abbrev)@ creates
-- a new derived unit, expressed in terms of @base_unit_type@ (which must be the
-- name of a /type/ not a /data constructor/). @ratio@ says how many base units
-- are in the derived unit. (Thus, if @unit_name@ is @"Minute"@ and @base_unit_type@
-- is @''Second@, then @ratio@ would be @60@.) @abbrev@, if supplied, becomes
-- the string produced in the derived unit's @Show@ instance. If no abbreviation
-- is supplied, no @Show@ instance is generated.
--
-- Example usage:
--
-- > $(declareDerivedUnit "Minute" ''Second 60 (Just "min"))
declareDerivedUnit :: String -> Name -> Rational -> Maybe String -> Q [Dec]
declareDerivedUnit unit_name_str base_unit ratio m_abbrev = do
  checkIsType base_unit
  show_instance <- maybeMkShowInstance unit_name m_abbrev
  unit_instance <- [d| instance Unit $unit_type where
                         type BaseUnit $unit_type = $base_unit_type
                         conversionRatio _ = ratio |]
  return $ (DataD [] unit_name [] [NormalC unit_name []] [])
           : unit_instance ++ show_instance
  where
    unit_name = mkName unit_name_str
    unit_type = return $ ConT unit_name
    base_unit_type = return $ ConT base_unit

-- | @declareMonoUnit unit_name (Just abbrev)@ creates a new derived unit,
-- intended for use without unit polymorphism. The same type stands for both
-- the unit and dimension, and the instance of 'DefaultUnitOfDim' is set up
-- accordingly. Use this function (with the 'Data.Metrology' imports) if you
-- don't want to bother with LCSUs and just want to get to work. The @abbrev@,
-- if supplied, creates an appropriate @Show@ instance.
--
-- > $(declareMonoUnit "Meter" (Just "m"))
--
-- produces all of the following
--
-- > data Meter = Meter
-- > instance Dimension Meter
-- > instance Unit Meter where
-- >   type BaseUnit Meter = Canonical
-- >   type DimOfUnit Meter = Meter
-- > type instance DefaultUnitOfDim Meter = Meter
-- > instance Show Meter where
-- >   show _ = "m"
--
-- After a declaration like this, you probably want
--
-- > type Length = MkQu_U Meter
--
-- This last line is /not/ generated, as it is easy enough for you to write,
-- and it involves a new name (@Length@).
declareMonoUnit :: String -> Maybe String -> Q [Dec]
declareMonoUnit unit_name_str m_abbrev = do
  show_instance <- maybeMkShowInstance unit_name m_abbrev
  dim_instance <- [d| instance Dimension $unit_type |]
  unit_instance <- [d| instance Unit $unit_type where
                         type BaseUnit $unit_type = Canonical
                         type DimOfUnit $unit_type = $unit_type |]
  default_instance <- [d| type instance DefaultUnitOfDim $unit_type = $unit_type |]
  return $ (DataD [] unit_name [] [NormalC unit_name []] [])
           : show_instance ++ dim_instance ++ unit_instance ++ default_instance
  where
    unit_name = mkName unit_name_str
    unit_type = return $ ConT unit_name
