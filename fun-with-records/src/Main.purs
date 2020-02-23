module Main where

import Prelude
import Rec(rec)

import Effect (Effect)
import Effect.Console (log)

data Example1 = Example1 { foo :: Int, bar :: String }

data Example2 = Example2 { baz :: Array Int, foo :: Boolean }

instance showExample1 :: Show Example1 where
  show (Example1 row) = "Example1 " <> show row
  
instance showExample2 :: Show Example2 where
  show (Example2 row) = "Example2 " <> show row
  
main :: Effect Unit
main = do
   log $ show $ rec Example1 "a" 1
   log $ show $ rec Example2 [1,2,3] false
