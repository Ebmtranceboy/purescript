module Data.Complex (module Data.Complex) where

import Prelude
import Data.Int(toNumber, round) as Int
import Math(sqrt, cos, sin, atan2)
import Math(pow) as Math

-- | Complex number defined by its real and imaginary parts
data Cartesian a = Cartesian a a

-- | Real part
real :: forall a. Cartesian a -> a
real (Cartesian x _) = x

-- | Imaginary part
imag :: forall a. Cartesian a -> a
imag (Cartesian _ y) = y

-- | Imaginary unit
i :: forall a. Semiring a => Cartesian a
i = Cartesian zero one

instance showCartesian :: (Show a, Ord a, Semiring a, Ring a) => Show (Cartesian a) where
  show (Cartesian a b) = show a <> (if b<zero then "-" <> show (negate b) else "+" <> show b) <> "i"  

instance eqCartesian :: Eq a => Eq (Cartesian a) where
  eq (Cartesian a b) (Cartesian c d) = a==c && b==d

instance semiringCartesian :: Ring a => Semiring (Cartesian a) where
  add (Cartesian a b) (Cartesian c d) = Cartesian (a+c) (b+d)
  zero = Cartesian zero zero
  mul (Cartesian a b) (Cartesian c d) = Cartesian (a*c-b*d) (a*d+b*c)
  one = Cartesian one zero

instance functorCartesian :: Functor Cartesian where
  map f (Cartesian x y) = Cartesian (f x) (f y)

instance ringCartesian :: Ring a => Ring (Cartesian a) where
  sub z1 z2 = add z1 (map (_ * (sub zero one)) z2)

-- | Conjugate
conjugate :: forall a. Ring a => Cartesian a -> Cartesian a
conjugate (Cartesian x y) = Cartesian x (negate y)

-- | Magnitude Squared
magnitudeSquared :: forall a. Ring a => Cartesian a -> a
magnitudeSquared z = real $ z * (conjugate z)

-- | Normalize to norm 1
normalize :: Cartesian Number -> Cartesian Number
normalize z = map (_ / (sqrt $ magnitudeSquared z)) z

instance commutativeRingCartesian :: CommutativeRing a => CommutativeRing (Cartesian a)

instance divisionRingCartesian :: DivisionRing a => DivisionRing (Cartesian a) where
  recip z = map (_ * (recip $ magnitudeSquared z)) (conjugate z)

instance euclideanRingCartesianNmber :: EuclideanRing (Cartesian Number) where
  degree _ = 1
  div z z' = z * (recip z')
  mod z z' = zero

instance euclideanRingCartesianInt :: EuclideanRing (Cartesian Int) where
  degree _ = 1
  div z z' = map Int.round $ div (map Int.toNumber z) (map Int.toNumber z')
  mod z z' = sub z $ mul z' $ div z z' 

-- | From radius and angle in radians
fromPolar :: Number -> Number -> Cartesian Number
fromPolar r theta = Cartesian (r * cos theta) (r * sin theta)

-- | Angle in radians
angle :: Cartesian Number -> Number
angle (Cartesian x y) = atan2 y x

-- | Real power of a complex
pow :: Cartesian Number -> Number -> Cartesian Number
pow z n = fromPolar (Math.pow (sqrt $ magnitudeSquared z) n) (angle z * n)


