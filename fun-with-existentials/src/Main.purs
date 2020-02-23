module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

newtype Showable = Showable (forall r. (forall a. Show a => a -> r) -> r)
--type Exists f = ∀ r. (∀ a. f a -> r) -> r
--newtype Showable =  Show a => Showable (Exists (a))
-- (Question: can we write this using the type Exists from above?)

mkShowable :: forall a. Show a => a -> Showable
mkShowable a = Showable (_ $ a)

instance showShowable :: Show Showable where
  show (Showable a) = a show

showables :: Array Showable
showables = [mkShowable 1, mkShowable unit, mkShowable "hello"]

main :: Effect Unit
main = do
  log $ show showables
