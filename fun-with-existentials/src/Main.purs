module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce(unsafeCoerce)

type ShowBoxF a = 
  { show :: a -> String
  , value :: a
  }

data ShowBox

mkShowBox :: forall a. ShowBoxF a -> ShowBox
mkShowBox = unsafeCoerce

unShowBox :: forall r. (forall a. ShowBoxF a -> r) -> ShowBox -> r
unShowBox = unsafeCoerce

box :: ShowBox
box = mkShowBox {show, value: 42}

example :: String
example = unShowBox (\{show, value} -> show value) box

main :: Effect Unit
main = do
  log example
  
