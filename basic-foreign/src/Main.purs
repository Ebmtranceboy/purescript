module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

foreign import mascarpone :: Int -> Number

main :: Effect Unit
main = do
  logShow $ mascarpone 242
