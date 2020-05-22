
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import LeibnizProof (type (~), symm, coerce, refl)

data Test a
  = I Int (a ~ Int)
  | B Boolean (a ~ Boolean)

int :: Int -> Test Int
int i = I i refl

bool :: Boolean -> Test Boolean
bool b = B b refl

eval :: forall a. Test a -> a
eval (I value proof) = coerce (symm proof) value
eval (B value proof) = coerce (symm proof) value

main :: Effect Unit
main = do
  log $ show $ eval $ int 5
  log $ show $ eval $ bool true
