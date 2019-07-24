module Data.Complex (module Data.Complex) where

import Prelude
data Cartesian a = Cartesian a a

instance eqCartesian :: Eq a => Eq (Cartesian a) where
  eq (Cartesian a b) (Cartesian c d) = a==c && b==d

instance showCartesian :: (Show a, Ord a, Semiring a, Ring a) => Show (Cartesian a) where
  show (Cartesian a b) = show a <> (if b<zero then "-" <> show (negate b) else "+" <> show b) <> "i"  


