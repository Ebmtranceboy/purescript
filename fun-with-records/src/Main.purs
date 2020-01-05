module Main where

import Prelude
import Rec(Example(..), rec)

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ rec Example "a" 1
