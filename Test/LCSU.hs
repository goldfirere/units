{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{- test for local coherent set of units concept. -}

import GHC.TypeLits


-- kind annotation for base dimensions
newtype BaseDimK a = BaseDim a

-- kind annotation for base units
newtype BaseUnitK a = BaseUnit a

{- I see. If we are going to use promoted kinds, we should not use the same name for the type and the constructor 
because they are confusing in naming the kinds.
-}

-- type level map
-- type TMap a b = [( a , b )]

newtype Qu (lcsu :: [(BaseDimK *, BaseUnitK *)]) (unit :: [(BaseUnitK *, Nat)])  (value :: *) = Qu value

data Length = Length
data Time = Time

data Meter = Meter
data Second = Second

x :: Qu '[] '[ '( BaseUnit Meter , 3) ] Double
x = Qu 3


main :: IO ()
main = do
  putStrLn "typechecks!"
  