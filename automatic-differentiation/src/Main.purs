module Main(module Main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array(uncons,(:),zipWith,replicate,(..),length)
import Data.Maybe(Maybe(..)) 
import Math as Math
import Data.Foldable(and)

data D a = D {value :: a, dual :: Array a}

instance showD :: Show a => Show(D a) where
  show (D x) = "D " <> show x.value <> " " <> show x.dual

instance functorD :: Functor D where
  map f (D {value, dual}) = D {value: f value, dual: map f dual}

zipWith' f l r [] [] = []
zipWith' f _ r a [] = map (flip f r) a
zipWith' f l _ [] b = map (f l) b
zipWith' f l r x y = 
  case uncons x of
    Just {head: a, tail:as} ->
       case uncons y of
         Just {head: b, tail: bs} ->
             (f a b) : zipWith' f l r as bs
         _ -> []
    _ -> []
 
plus :: forall a. Semiring a => Array a -> Array a -> Array a
plus = zipWith' (+) zero zero

infixl 5 plus as .+

minus :: forall a. Ring a => Semiring a => Array a -> Array a -> Array a
minus = zipWith' (-) zero zero

infixl 5 minus as .-

times :: forall a. Semiring a => a -> Array a -> Array a
times a = map (a*_)

infixl 6 times as .*

semit :: forall a. Semiring a => Array a -> a -> Array a
semit a b = map (b*_) a

infixl 6 semit as *.

equal :: forall a. Eq a => Semiring a => Array a -> Array a -> Boolean
equal a b = and $ zipWith' (==) zero zero a b

infixl 4 equal as .=

instance eqD :: (Eq a, Semiring a) => Eq (D a) where
  eq (D {value:x, dual: x'}) (D {value:y, dual: y'}) = x==y && x'.=y'
  
instance semiringD :: Semiring a => Semiring (D a) where
  add (D {value:a, dual:a'}) (D {value:b, dual:b'}) 
     = D {value: a+b, dual: a' .+ b'}
  mul (D {value:a, dual:a'}) (D {value:b, dual:b'}) 
     = D {value:a*b, dual: a.*b' .+ a'*.b}
  zero = D {value:zero, dual: [zero]}
  one = D {value:one, dual:[zero]}

instance ringD :: Ring a => Ring (D a) where
  sub (D {value:a, dual:a'}) (D {value:b, dual:b'}) = 
     D {value: a-b, dual: a' .- b'}

instance divisionRingD :: DivisionRing a => DivisionRing (D a) where
  recip (D {value:a, dual: a'}) = 
    let s = recip a
    in D {value:s, dual: map negate $ a' *. (s*s)}
    
instance commutativeRingD :: CommutativeRing a => CommutativeRing (D a)

instance euclideanRingDNumber :: EuclideanRing (D Number) where
  degree _ = 1
  div z z' = z * (recip z')
  mod z z' = zero

sin :: D Number -> D Number
sin (D {value:a, dual: a'}) = 
   D  {value: Math.sin a, dual: Math.cos a .* a'}

sqrt :: D Number -> D Number
sqrt (D {value:a, dual: a'}) = 
   let sqrta = Math.sqrt a
   in D {value: sqrta, dual: (recip $ 2.0 * sqrta) .* a'}

d :: forall a. Semiring a => Int -> D a
d n = D {value: zero, dual: replicate n zero <> [one]}

diff' :: forall a b. Semiring a => (Array (D a) -> b) -> Array (D a) -> b
diff' f vars = f $ zipWith (+) vars $ map d $ 0..length vars

--diff :: forall a b. Semiring a => (Array (D a) -> b) -> Array (D a) -> Array b
diff f vars 
  | D {value, dual} <- f $ zipWith (+) vars $ map d $ 0..length vars =
     case uncons dual of
     Just {head, tail} -> head
     _ -> zero

{-


instance (Num a,Fractional a) => Fractional (R a) where
   fromRational a = R (fromRational a) []
   (R a a') / (R b b') = let s = 1/b in
       R (a*s) ((a'*.b.-a.*b') *. (s*s))

instance Floating a => Floating (R a) where
   exp (R a a') = let e = exp a in R e (e .* a')
   log (R a a') = R (log a) ((1/a) .* a')
   cos (R a a') = R (cos a) (-sin a .* a')
   sinh (R a a') = R (sinh a) (cosh a .* a')
   cosh (R a a') = R (cosh a) (sinh a .* a')
   asin (R a a') = R (asin a) ((1/(sqrt $ 1 - a^2)) .* a')
   atan (R a a') = R (atan a) ((1/(a^2+1)) .* a')
   acos (R a a') = R (acos a) ((-1/(sqrt $ 1 - a^2)) .* a')
   asinh (R a a') = R (asinh a) ((1/(sqrt $ 1 + a^2)) .* a')
   acosh (R a a') = R (acosh a) ((1/(sqrt $ a^2 - 1)) .* a')
   atanh (R a a') = R (atanh a)  ((1/(1-a^2)) .* a')
   pi = R (realToFrac pi) []

instance (Num a,Ord a) => Ord (R a) where
  (R x _) < (R y _) = x < y

d n = R 0 (replicate n 0 ++ [1])

g [x, y] = (x+2*y)^2/(x+y)

diff f vars = dual $ f $ zipWith (+) vars $ map d [0..]
image f vars = value $ f $ zipWith (+) vars $ map d [0..] 
		
		-- retourne [a,b,c...] les coeff de l'equation de l'hyperplan tangent au point vars
plane f vars =  -- tels que ax+by+cz+...=0
  let grad = diff f vars
	in grad ++ [-1,negate $ subtract (image f vars) $ sum $ zipWith (*) grad $ map value vars]

parallel [a,b,_] [a',b',_] = a*b' == a'*b
interLine ([a,b,c], [a',b',c']) = ((b*c'-b'*c)/(a*b'-a'*b),(a'*c-a*c')/(a*b'-a'*b))

test = diff (\[x,y]->x^3+y^2+x*y) [2, 5] == [17,12]



-}
main :: Effect Unit
main = do
  log "Hello sailor!"
