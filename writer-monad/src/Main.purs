module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

import Control.Monad.Writer(Writer, execWriter, runWriter)
import Control.Monad.Writer.Class(tell)

import Data.Traversable(traverse_)
import Data.Monoid.Additive(Additive(..))

import Data.Newtype(unwrap)

sumWriter :: Array Int -> Writer (Additive Int) Unit
sumWriter arr = traverse_ (tell <<< Additive) arr

sumArray :: Array Int -> Int
sumArray arr = unwrap $ execWriter $ sumWriter arr

collatzWriter :: Int -> Writer (Array Int) Int
collatzWriter n = do
  tell [n]
  if n == 1 
    then pure 0
    else 
      if n `mod`2 == 0 
        then (_+1) <$> collatzWriter (n `div`2)
        else (_+1) <$> collatzWriter (3*n+1)

main :: Effect Unit
main = do
  logShow $ sumArray [1,3,6,9]
  logShow $ runWriter $ collatzWriter 10
