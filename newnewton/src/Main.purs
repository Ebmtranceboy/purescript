module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Math (pow)
import Data.Array(length, range, tail, (!!))
import Data.Int (toNumber)
import Data.Foldable(foldr)
import Partial.Unsafe(unsafePartial)
import Data.Maybe(fromJust)

zeros = [1.0, 4.0, 7.0, 7.0] :: Array Number
poles = [2.0, 5.0, 9.0, 14.0] :: Array Number

nz = length zeros :: Int
np = length poles :: Int

-- ASSUMING np >= nz:

f :: Int -> Array Number -> Array Number
f n xs = (\z -> pow z (toNumber n)) <$> xs

g :: Int -> Number
g n = foldr add 0.0 (f n zeros) - foldr add 0.0 (f n poles)

nth :: Array Number -> Int -> Number
nth xs n = unsafePartial $ fromJust $ xs !! (n-1) 
infix 8 nth as !

j :: Array Number
j = g <$> range 1 (nz+np)

h :: Int -> Int -> Int
h _ 1 = 1
h n i = (n - 2) * h (n-1) (i-1)

makeK :: Int -> Array Number
makeK 1 = [1.0]
makeK n = 
  let k' = makeK $ n - 1
   in k' <> [foldr add 
                   0.0 
                   $ (\i -> (toNumber $ h n i) * j!i * k'!(n-i)) <$> range 1 (n-1)]

k :: Array Number
k = unsafePartial $ fromJust $ tail $ makeK $ nz+np+1

s :: Int -> Int -> Number
s 0 _ = - (j!1)
s 1 p = 2.0 * k! (p+1) / ((toNumber $ p+1) * k! p) - j!1
s z p = 
  let s10 = s (z-1) p
      s1m1 = s (z-1) (p-1)
      s1p1 = s (z-1) (p+1)
   in (s10 * (s1m1 - s1p1) + s (z-2) p * (s1p1 - s10)) / (s1m1 - s10) 

main :: Effect Unit
main = do
  log $ show $ s nz np
  log $ show $ foldr add 0.0 (zeros <> poles)
