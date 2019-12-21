module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Array(snoc)
import Control.Monad.State(State(..), runState)
import Data.Tuple(fst)
import Control.Monad.State.Class(class MonadState, get, modify)

put :: forall a st. Functor st => MonadState (Array a) st => a -> st Unit
put x = void $ modify (flip snoc x)

fromSeq :: forall a. State (Array a) (Array a) -> Array a
fromSeq seq = fst $ runState seq []

seqArray :: State (Array Int) (Array Int)
seqArray = do
  put 2
  put 3
  put 4
  get
  

--instance monadB :: Monad B where
--  pure = 

--f x a = (flip snoc x) <$> (B a)

--arr :: Array Int
{-arr = do
  let b = B[]
  a <- (flip snoc 1) <$> b
  a <- (flip snoc 2) <$> (B a)
  a <- (flip snoc 6) <$> (B a)
  a <- f 5 a
  B a
-}
main :: Effect Unit
main = do
  pure unit
  

