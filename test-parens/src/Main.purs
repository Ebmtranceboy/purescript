module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Control.Monad.State(State, execState)
import Control.Monad.State.Class(modify)

import Data.String.CodeUnits(toCharArray)

type Record = {left:: Int, right:: Int, soFar:: Boolean}

parseArray :: Array Char -> State Record _
parseArray = traverse_ \c -> modify \rec -> 
             let now = conj rec.soFar (rec.right <= rec.left)
             in case c of
                  '(' -> rec { left = rec.left + 1, soFar = now}
                  ')' -> rec { right = rec.right + 1, soFar = now}
                  otherwise -> rec

testParens :: String -> Boolean
testParens str = 
  let state = execState (parseArray (toCharArray str)) {left:0, right:0, soFar:true}
  in conj state.soFar $ state.right == state.left 
  
main :: Effect Unit
main = do
  log "Hello sailor!"
