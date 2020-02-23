module Main where

import Prelude
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Console (log)

class Profunctor p where
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d

data Star f a b = Star (a -> f b)

runStar :: forall f a b. Star f a b -> a -> f b
runStar (Star star) = star

instance profStar :: Functor f => Profunctor (Star f) where
  dimap f g (Star star) = Star (map g <<< star <<< f)
main :: Effect Unit
main = do
  log $ show $ runStar (dimap (_+1) (_*2) (Star (\x -> Just x))) 10
