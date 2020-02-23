module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array

unfld :: forall a. Int -> ( a -> a) -> a -> Array a
unfld 0 _ _ = []
unfld n f x = x : unfld (n-1) f (f x)

main :: Effect Unit
main = do
  log $ show $ unfld 5 (_*0.8) 125.0
