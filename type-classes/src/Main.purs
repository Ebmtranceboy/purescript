module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Data.Foldable(class Foldable, foldMap, foldl, foldr)
import Data.Array((..))

-- testing Instance Dependencies

data OneMore f a = OneMore a (f a)
 
instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldl g u (OneMore x seq) = g (foldl g u seq) x
  foldr g u (OneMore x seq) = g x $ foldr g u seq
  foldMap g (OneMore x seq) = g x <> (foldMap g seq)

arr :: Array Int
arr = [2, 4, 4]
oneMore :: OneMore Array Int
oneMore = OneMore 5 arr  

-- testing Multi Parameter Type Classes

-- say we have a class:

class Monoid m <= Action m a where
   act :: m -> a -> a

-- with 2 laws:

-- act mempty a = a
-- act (m1 <> m2) a = act m1 (act m2 a)

-- first code the constrains:

newtype Multiply = Multiply Int

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

-- then the behavior expected:

instance repeatAction :: Action Multiply String where
  act (Multiply n) str = foldMap (const str) $ 1..n

-- extended to an array of a:

instance extension :: Action m a => Action m (Array a) where
  act m = map (act m)

-- funny extension  

newtype Self m = Self m

instance showSelf :: Show m => Show (Self m) where
  show (Self m) = "Self" <> show m

instance selfAction :: Monoid m => Action m (Self m) where
  act m (Self n) = Self $ m <> n


instance showMultiply :: Show Multiply where
  show (Multiply n) = " *" <> show n

main :: Effect Unit
main = do
  logShow $ foldr (+) 0 oneMore
  logShow $ act (Multiply 2) $ act (Multiply 3) "ab"
  logShow $ act (Multiply 2 <> Multiply 3) "ab"
  logShow $ act (Multiply 2) ["ab", "cde", "f"]
  logShow $ act (Multiply 2) [Self (Multiply 1), Self (Multiply 3)]
