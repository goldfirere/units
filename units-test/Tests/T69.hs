{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Tests.T69 ( tests ) where
import Data.Metrology.Poly
import Data.Ratio hiding ( (%) )
import Numeric.Natural
import Test.Tasty
import Test.Tasty.HUnit

data CurrencyRelation = Base | Quote

data MoneyDim (crel :: CurrencyRelation)

instance Dimension (MoneyDim 'Base)

instance Dimension (MoneyDim 'Quote)

data MoneyBaseAmt = MoneyBaseAmt

instance Unit MoneyBaseAmt where
  type BaseUnit MoneyBaseAmt = Canonical
  type DimOfUnit MoneyBaseAmt = MoneyDim 'Base

instance Show MoneyBaseAmt where
  show = const "MoneyBaseAmt"

type LCSU' =
  MkLCSU
    '[ (MoneyDim 'Base, MoneyBaseAmt)
     ]

type MoneyBase' =
  MkQu_DLN (MoneyDim 'Base) LCSU' (Ratio Natural)

tests :: TestTree
tests = testGroup "T69"
  [ testCase "SubtractNatural" $ ((5 % MoneyBaseAmt :: MoneyBase') |-| (3 % MoneyBaseAmt :: MoneyBase')) # MoneyBaseAmt @?= 2 ]

