module Main(module Main) where

import Prelude

import Data.Array (length, replicate, uncons, zipWith, (..), (:))
import Data.Foldable (and, sum)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Math as Math
import Partial.Unsafe (unsafePartial)

data D a = D {value :: a, dual :: Array a}

instance showD :: Show a => Show(D a) where
  show (D x) = "D " <> show x.value <> " " <> show x.dual

instance functorD :: Functor D where
  map f (D {value, dual}) = D {value: f value, dual: map f dual}

zipWith' :: forall a b c. (a -> b -> c) -> a -> b -> Array a -> Array b -> Array c
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

instance euclideanRingDNumber :: (CommutativeRing a, DivisionRing a) => EuclideanRing (D a) where
  degree _ = 1
  div z z' = z * (recip z')
  mod z z' = zero

sin :: D Number -> D Number
sin (D {value:a, dual: a'}) = 
   D  {value: Math.sin a, dual: Math.cos a .* a'}

cos :: D Number -> D Number
cos (D {value:a, dual: a'}) = 
   D  {value: Math.cos a, dual: -Math.sin a .* a'}

tan :: D Number -> D Number
tan (D {value:a, dual: a'}) = 
   D  {value: Math.tan a, dual: (1.0 / (Math.cos a * Math.cos a)) .* a'}

sqrt :: D Number -> D Number
sqrt (D {value:a, dual: a'}) = 
   let sqrta = Math.sqrt a
   in D {value: sqrta, dual: (recip $ 2.0 * sqrta) .* a'}
   
exp :: D Number -> D Number
exp (D {value:a, dual: a'}) = 
   let expa = Math.exp a
   in D {value: expa, dual: expa .* a'}

log :: D Number -> D Number
log (D {value:a, dual: a'}) = 
  D {value: Math.log a, dual: (1.0/a) .* a'} 


asin :: D Number -> D Number
asin (D {value:a, dual: a'}) = 
  D {value: Math.asin a, dual: (1.0/Math.sqrt(1.0-a*a)) .* a'} 

acos :: D Number -> D Number
acos (D {value:a, dual: a'}) = 
  D {value: Math.acos a, dual: (-1.0/Math.sqrt(1.0-a*a)) .* a'} 

atan :: D Number -> D Number
atan (D {value:a, dual: a'}) = 
  D {value: Math.atan a, dual: (1.0/(1.0+a*a)) .* a'} 

mathsinh :: Number -> Number
mathsinh x = (Math.exp x - Math.exp (-x)) / 2.0

mathcosh :: Number -> Number
mathcosh x = (Math.exp x + Math.exp (-x)) / 2.0

mathtanh :: Number -> Number
mathtanh x = (Math.exp x - Math.exp (-x)) / (Math.exp x + Math.exp (-x))

sinh :: D Number -> D Number
sinh (D {value:a, dual: a'}) = 
  D {value: mathsinh a, dual: (mathcosh a) .* a'} 

cosh :: D Number -> D Number
cosh (D {value:a, dual: a'}) = 
  D {value: mathcosh a, dual: (mathsinh a) .* a'} 

tanh :: D Number -> D Number
tanh (D {value:a, dual: a'}) = 
  D {value: mathtanh a, dual: (1.0 / (mathcosh a * mathcosh a)) .* a'} 

{-

asinh :: D Number -> D Number
asinh (D {value:a, dual: a'}) = 
  D {value: Math.asinh a, dual: (1.0/Math.sqrt(1.0+a*a)) .* a'} 
   
acosh :: D Number -> D Number
acosh (D {value:a, dual: a'}) = 
  D {value: Math.acosh a, dual: (1.0/Math.sqrt(a*a-1.0)) .* a'} 

atanh :: D Number -> D Number
atanh (D {value:a, dual: a'}) = 
  D {value: Math.atanh a, dual: (1.0/(1.0-a*a)) .* a'} 
  -}

d :: forall a. Semiring a => Int -> D a
d n = D {value: zero, dual: replicate n zero <> [one]}

diffs :: forall a b. Semiring a => (Array (D a) -> b) -> Array (D a) -> b
diffs f vars = f $ zipWith (+) vars $ map d $ 0..length vars

diff1 :: forall a b. Semiring b => Semiring a => (Array (D b) -> D a) -> Array (D b) -> a
diff1 f vars 
  | D {value, dual} <- f $ zipWith (+) vars $ map d $ 0..length vars =
     case uncons dual of
     Just {head, tail} -> head
     _ -> zero

-- retourne [a,b,c...] les coeff de l'equation de l'hyperplan tangent au point vars
-- tels que ax+by+cz+...=0
plane :: (Array (D Number) -> D Number) -> Array (D Number) -> Array Number
plane f vars = 
  let D{value, dual: grad} = diffs f vars
   in grad <> [-1.0, value - (sum $ zipWith (*) grad $ ((_.value) <<< (\(D x) -> x)) <$> vars)]

main :: Effect Unit
main = do
  logShow $ diffs (unsafePartial $ \[x,y]-> (x+one)/(y-one)) [D{value: 2.0,
                     dual:[]},D{value:5.0,dual:[]}] == D {value:0.75,dual:[0.25,-0.1875]}
  logShow $ diffs (unsafePartial $ \[x,y]-> x*x*x+y*y+x*y) [D{value: 2.0,
                     dual:[]},D{value:5.0,dual:[]}] == D {value:43.0,dual:[17.0,12.0]}
