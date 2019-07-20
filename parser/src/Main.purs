module Main where

import Prelude
import Parser(Parser, runParser, split, string, upper, upperOrLower)

import Effect(Effect)
import Effect.Console(logShow)

import Data.Array(many)
import Control.Alternative((<|>))

manyAsAndBs :: Parser (Array String)
manyAsAndBs = do
  as <- many $ string "a"
  bs <- many $ string "b"
  pure $ as <> bs

severalAsOrBs :: Parser (Array String)
severalAsOrBs = many (string "a" <|> string "b")
  

main :: Effect Unit
main = do
  logShow $ runParser split "test"
  logShow $ runParser (many split) "test"
  logShow $ runParser (string "tes") "test"
  logShow $ runParser upper "test"
  logShow $ runParser upper "TEST"
  logShow $ runParser upperOrLower "abcDEF"
  logShow $ runParser (many upperOrLower) "abCDeFgh"
  logShow $ runParser manyAsAndBs "bbcdeaaa"
  logShow $ runParser manyAsAndBs "aaabbcde"
  logShow $ runParser manyAsAndBs "aaabbaba"
  logShow $ runParser severalAsOrBs "aaabbaba"
  
