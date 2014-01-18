{-# LANGUAGE DataKinds, FlexibleInstances, FlexibleContexts, KindSignatures, MultiParamTypeClasses, TypeFamilies #-}
{- test for local coherent set of units concept. -}

import GHC.TypeLits
import Text.Printf

-- type level integer
data ZahlK a = Plus a | Minus a

-- kind annotation for base dimensions
data BaseDimK star = BaseDim star

-- kind annotation for base units
data BaseUnitK star = BaseUnit star

{- If we are going to use promoted kinds, we should not use the same name for the type and the constructor 
because they are confusing in naming the kinds. And type synonyms does not work.
-}
type Zahl = ZahlK Nat 
type TMap a b = [( a , b )]


{- The representation of a quantity consists of 
   The constructor Qu,
   The local coherent system of units lcsu,
   The unit of the quantity,
   and the numerical value.

   Our convention is that the internal representation of the numerical value of a quantity is 
   in the LCSU, not in terms of the units.
   Units are used whenever we try to view the actual value.

   I thought we wanted one more argument to Qu, namely (dim :: [(BaseDimK *, ZahlK Nat)]),
   But since the dim is always uniquely determined from the unit, I'm thinking we might not need it.

-}

newtype Qu (lcsu :: [(BaseDimK *, BaseUnitK *)]) (unit :: [(BaseUnitK *, ZahlK Nat)])  (value :: *) = Qu value
  deriving (Eq, Show)


{-
  coherentDerivedUnit :: LCSU -> Dim -> Unit

  given LCSU, convents the given dimension to the coherent derived unit under the LCSU.
-}
type family CoherentDerivedUnit 
     (lcsu    :: [(BaseDimK *, BaseUnitK *)]) 
     (dim     :: [(BaseDimK *, ZahlK Nat)])
     :: ([(BaseUnitK *, ZahlK Nat)]) where
  CoherentDerivedUnit SI LengthDim = '[ '( BaseUnit Meter , Plus 1) ]
  CoherentDerivedUnit SI SpeedDim = '[ '( BaseUnit Meter , Plus 1), '(BaseUnit Second, Minus 1) ]
  CoherentDerivedUnit SI TimeDim = '[ '( BaseUnit Second , Plus 1) ]

  CoherentDerivedUnit CGS LengthDim = '[ '( BaseUnit (Centi Meter) , Plus 1) ]
  CoherentDerivedUnit CGS SpeedDim = '[ '( BaseUnit (Centi Meter) , Plus 1), '(BaseUnit Second, Minus 1) ]
  CoherentDerivedUnit CGS TimeDim = '[ '( BaseUnit Second , Plus 1) ]


-- some base dimension names
data Length = Length
data Time = Time
data Current = Current

-- dimensions are monominals of the base dimension
type LengthDim = '[ '( BaseDim Length , Plus 1) ] 
type SpeedDim = '[ '( BaseDim Length , Plus 1) ,  '( BaseDim Time , Minus 1) ] 
type TimeDim = '[ '( BaseDim Time , Plus 1) ]  

-- some units
data Parsec = Parsec
data Meter = Meter
data Second = Second
data Year = Year
data Centi u = Centi u
data Nano u = Nano u
data Ampere = Ampere

-- some LCSUs
type SI = '[ '( BaseDim Length, BaseUnit Meter),'( BaseDim Time, BaseUnit Second)  ]
type CGS = '[ '( BaseDim Length, BaseUnit (Centi Meter)),'( BaseDim Time, BaseUnit Second)  ]
type CGSA = '[ '( BaseDim Length, BaseUnit (Centi Meter)),'( BaseDim Time, BaseUnit Second), '(BaseDim Current, BaseUnit Ampere)  ]


{- several different representations of marathon track distance, 42.195km . -}
distance0 :: Qu SI '[ '( BaseUnit Meter , Plus 1) ] Double
distance0 = Qu 42195

distance1 :: Qu CGS '[ '( BaseUnit (Centi Meter), Plus 1) ] Double
distance1 = Qu 4219500

-- note here that the internal numerical value is in SI unit, not in centi meters.
-- but if you try to print this value the library will print it in cm.
distance2 :: Qu SI '[ '( BaseUnit (Centi Meter), Plus 1) ] Double
distance2 = Qu 42195

-- same here.
distance3 :: Qu CGS '[ '( BaseUnit Meter, Plus 1) ] Double
distance3 = Qu 4219500

velocity0 :: Qu SI '[ '( BaseUnit Meter , Plus 1),  '( BaseUnit Second , Minus 1) ] Double
velocity0 = Qu 10.44

velocity1 :: Qu CGS '[ '( BaseUnit (Centi Meter) , Plus 1),  '( BaseUnit Second , Minus 1) ] Double
velocity1 = Qu 1044

velocity2 :: Qu CGS '[ '( BaseUnit (Nano Parsec), Plus 1),  '( BaseUnit Year, Minus 1) ] Double
velocity2 = Qu 10.68


-- This should really use ekmett's lens in the future
-- (sadly, it doesn't compile on current ghc head.)
class ValueLens q a where  
  toValue  :: q a -> a
  fromValue :: a -> q a
  
instance ValueLens (Qu SI '[  '( BaseUnit Meter, Plus 1)]) a where
  toValue (Qu v) = v
  fromValue v = Qu v
instance ValueLens (Qu CGS '[  '( BaseUnit (Centi Meter), Plus 1)]) a where
  toValue (Qu v) = v
  fromValue v = Qu v
instance (Fractional a) => ValueLens (Qu SI '[  '( BaseUnit (Centi Meter), Plus 1)]) a where
  toValue (Qu v) = v*100
  fromValue v = Qu (v/100)
instance (Fractional a) => ValueLens (Qu CGS '[  '( BaseUnit Meter, Plus 1)]) a where
  toValue (Qu v) = v/100
  fromValue v = Qu (v*100)

instance ValueLens (Qu SI '[  '( BaseUnit Second, Plus 1)]) a where
  toValue (Qu v) = v
  fromValue v = Qu v
instance ValueLens (Qu CGS '[  '( BaseUnit Second, Plus 1)]) a where
  toValue (Qu v) = v
  fromValue v = Qu v

-- class for unit conversions
class Convertible a b where
  convert :: a -> b
  
-- We can always upconvert to a larger LCSU.  
instance Convertible (Qu CGS u v) (Qu CGSA u v) where
  convert (Qu v) = Qu v

-- We can convert between different LCSU if the destination LCSU is no smaller than the source
instance (Fractional v) => 
         Convertible (Qu CGS '[ '( BaseUnit Meter, Plus 1) ] v) (Qu SI '[ '( BaseUnit Meter, Plus 1) ] v) where
  convert (Qu v) = Qu (v/100)
instance (Fractional v) => 
         Convertible (Qu CGS '[ '( BaseUnit Meter, Plus 1) ] v) (Qu SI '[ '( BaseUnit (Centi Meter), Plus 1) ] v) where
  convert (Qu v) = Qu (v/100)
instance (Fractional v) => 
         Convertible (Qu CGS '[ '( BaseUnit (Centi Meter), Plus 1) ] v) (Qu SI '[ '( BaseUnit Meter, Plus 1) ] v) where
  convert (Qu v) = Qu (v/100)
instance (Fractional v) => 
         Convertible (Qu CGS '[ '( BaseUnit (Centi Meter), Plus 1) ] v) (Qu SI '[ '( BaseUnit (Centi Meter), Plus 1) ] v) where
  convert (Qu v) = Qu (v/100)
instance (Convertible (Qu SI '[ '( BaseUnit Meter, Plus 1), '(BaseUnit Second, Minus 1)] Double)
                      (Qu CGS '[ '( BaseUnit (Centi Meter), Plus 1), '( BaseUnit Second, 'Minus 1)] Double)) where
  convert (Qu v) = Qu (v*100)                                                                                         



-- We can also convert between units within the same LCSU. 
-- We never need arithmetics in such cases,
-- because our convention is that the internal numerical value is measured in the LCSU.

instance  Convertible (Qu SI '[ '( BaseUnit Meter, Plus 1) ] v) (Qu SI '[ '( BaseUnit (Centi Meter), Plus 1) ] v) where
  convert (Qu v) = Qu v
instance  Convertible (Qu SI '[ '( BaseUnit (Centi Meter), Plus 1) ] v) (Qu SI '[ '( BaseUnit Meter, Plus 1) ] v) where
  convert (Qu v) = Qu v


{- 
Laws of physics must be invariant over different unit representations (unit polymorphism.)
However, for most of the usecase it's OK to assume that quantities involved belong to unique CSU.
Most scientific programs are written in this manner, and the assumption has practical merits.
This is how I am going to realize the unit polymorphism. -}
  
lawOfTravel :: (Fractional val)
  => Qu csu (CoherentDerivedUnit csu LengthDim) val 
  -> Qu csu (CoherentDerivedUnit csu SpeedDim) val
  -> Qu csu (CoherentDerivedUnit csu TimeDim) val
lawOfTravel (Qu x) (Qu v) = Qu (x/v)



main :: IO ()
main = do
  printf "The marathon distance is %f meters.\n" $ toValue distance0
  printf "Or, it is %f centi meters.\n" $ toValue distance2
  printf "Usain Bolt will run it in %f seconds.\n" $ toValue $ lawOfTravel distance0 velocity0
  
  -- GHC can infer the correct LCSU and units to convert to!
  printf "Or, in %f seconds.\n" $ toValue $ lawOfTravel (convert distance1) velocity0
  printf "Or, in %f seconds.\n" $ toValue $ lawOfTravel (convert distance2) velocity0
  printf "Or, in %f seconds.\n" $ toValue $ lawOfTravel (convert distance3) velocity0
  
  -- The last calculation is performed in CGS not in SI, thus you might observe some truncation error
  -- due to the use of the different CSU.
  printf "Or, in %f seconds.\n" $ toValue $ lawOfTravel distance1 (convert velocity0)
  
  putStrLn "typechecks!"
  