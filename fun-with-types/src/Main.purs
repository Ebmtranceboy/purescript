module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Traversable (for_)

import ExtractKeys (extractKeys)
import Record.Extra (keys)

main :: Effect Unit
main = do
  for_ (extractKeys {a: 1, b: true, c: "hi"})
    (\k -> log $ show k)
  for_ (keys {a: 1, b: true, c: "hi"})
    (\k -> log $ show k)
   