module Euler010  where
 
import Prelude
 
import Control.Monad.ST (ST, for, run)
import Data.Array (drop, filter, (..))
import Data.Array.ST (STArray, peek, poke, withArray)
import Data.Foldable (sum)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Math (sqrt)
 
floorF :: (Number -> Number) -> Int -> Int
floorF f = floor <<< f <<< toNumber
 
crosser :: forall h. Int -> STArray h Int -> ST h Unit
crosser n a =
  let sn = 1 + floorF sqrt n 
  in for 2 sn \i -> do
       v <- peek i a
       when (v == Just i) do
         let u = 1 + n / i
         for i u \j -> do
            poke (j * i) 0 a
 
genTable :: Int -> Array Int
genTable n = run (withArray (crosser n) (0..n))
 
genPrimes :: Int -> Array Int
genPrimes n = filter (_ /= 0) $ drop 2 $ genTable n
 
euler10 :: Int -> Int
euler10 n =
  let primes = genPrimes (n - 1)
  in sum primes
