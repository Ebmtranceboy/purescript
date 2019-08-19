module Main where

import Control.Comonad
import Data.String
import Data.Tuple
import Env
import Prelude

import Data.Array (replicate)
import Effect (Effect)
import Effect.Console (log)

type Settings = { padAmount :: Int
                , maxLength :: Int
                , padChar :: Char
                }

getPadChar :: forall a. Env Settings a -> Char 
getPadChar w = asks (_.padChar) w

context :: Env Settings String
context = Env ( { padAmount: 3
                , maxLength: 5
                , padChar: '*'}) "Hello World"

trunc :: Env Settings String -> String
trunc w = 
  let mxLngth = asks (_.maxLength) w
   in take mxLngth (extract w)

pad :: Env Settings String -> String
pad = do
  let padding = fromCodePointArray <$>
                  (replicate <$> asks (_.padAmount) 
                             <*> asks (codePointFromChar <<< (_.padChar)))
  padding <> extract <> padding

setPadChar :: Char -> Settings -> Settings
setPadChar c set = set{padChar = c}

main :: Effect Unit
main = do
  log $ show $ ask $ Env 42 "Hello sailor!"
  log $ show $ asks fst (Env (Tuple "first" "second") 1337) 
  log $ show $ getPadChar context
  log $ show $ asks (_.padAmount) context
  log $ trunc context
  log $ pad context
  log $ trunc <<< (extend pad) $ context
  log $ pad =>= trunc $ context
  log $ trunc =>= pad $ context
  log $ trunc =>= pad <<< local (setPadChar '_') =>= pad $ context


