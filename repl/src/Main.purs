module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref

main :: Effect Unit
main = do
  let a = 6
      b = 5
  rc <- new ([] :: Array Number)
  write [3] rc
  c <- read rc
      
  log $ show $ [a + b] <> c
  pure unit
  

