module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Control.Monad.ST.Ref (new, read, modify)
import Control.Monad.ST (ST, run)
import Control.Monad.ST.Internal(for)
import Data.Int(floor)

simulate' :: forall t.Number -> Number -> Number -> ST t Number
simulate' x0 v0 time = do
  ref <- new { x: x0, v: v0 }
  for 0 (floor $ time * 1000.0) \_ -> do
    _ <- modify (\o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }) ref
    pure unit
  final <- read ref
  pure final.x

simulate :: Number -> Number -> Number -> Number
simulate x0 v0 time = run (simulate' x0 v0 time)

main :: Effect Unit
main = do
  n <- random
  logShow n
