module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

import Control.Monad.Writer(Writer, execWriter)
import Control.Monad.Writer.Class(tell)

import Data.Traversable(traverse_)
import Data.Monoid.Additive(Additive(..))

import Data.Newtype(unwrap)

sumWriter :: Array Int -> Writer (Additive Int) Unit
sumWriter arr = traverse_ (tell <<< Additive) arr

sumArray :: Array Int -> Int
sumArray arr = unwrap $ execWriter $ sumWriter arr

main :: Effect Unit
main = do
  logShow $ sumArray [1,3,6,9]
