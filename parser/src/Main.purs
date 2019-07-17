module Main where

import Prelude
import Parser(runParser, split, string, upper, upperOrLower)

import Effect(Effect)
import Effect.Console(logShow)

import Data.Array(many)

main :: Effect Unit
main = do
  logShow $ runParser split "test"
  logShow $ runParser (many split) "test"
  logShow $ runParser (string "tes") "test"
  logShow $ runParser upper "test"
  logShow $ runParser upper "TEST"
  logShow $ runParser upperOrLower "abcDEF"
  logShow $ runParser (many upperOrLower) "abCDeFgh"
