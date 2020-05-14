module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Record as Record
import Type.Row (type (+))

type Env2 r
  = ( x :: String
    | r
    )

type Env3 r
  = ( f :: String -> Int
    | r
    )

env2 :: { | Env2 () }
env2 =
  { x: "a"
  }

env3 :: { | Env3 () }
env3 =
  { f: const 2
  }

env_2_3 :: { | Env2 + Env3 + () }
env_2_3 = Record.merge env2 env3

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
