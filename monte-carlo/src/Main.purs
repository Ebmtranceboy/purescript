module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Rand(rand)
import Data.Array(elem, (:))
import Data.Foldable(foldl)
import Data.Int(toNumber)

type Random = {val :: Int, gen :: Int, seed :: Int}
type Experience a = {st :: Random, u :: a}
type E a = Experience Unit -> Experience a

rollDie :: E Int
rollDie {st} = {st: rand st, u: st.val `mod` 6 + 1}

sumTwoDice :: E Int
sumTwoDice {st} = 
  let d1 = rollDie {st, u: unit}
      d2 = rollDie {st: d1.st, u: unit}
   in {st: d2.st, u: d1.u + d2.u}
 
playCrapsOnce :: E Boolean
playCrapsOnce {st} =
  let firstRoll = sumTwoDice{st, u: unit}
  in if firstRoll.u `elem` [7,11] 
     then {st: firstRoll.st , u: true}
     else if firstRoll.u `elem` [2,3,12]
           then {st: firstRoll.st, u: false}
           else loopCraps firstRoll firstRoll

loopCraps :: Experience Int -> Experience Int -> Experience Boolean
loopCraps firstRoll roll = 
  let newRoll = sumTwoDice {st: roll.st, u: unit}
   in if newRoll.u == firstRoll.u
       then {st: newRoll.st, u: true}
       else if newRoll.u == 7
            then {st: newRoll.st, u: false}
            else loopCraps firstRoll newRoll

computeHouseEdge :: Int -> E Boolean -> Number
computeHouseEdge numTrials game =
  let env = {st: rand {val: 1, gen: 1, seed: 517}, u: unit}
      count = foldl (+) 0 $ (if _ then 1 else -1 ) <$> loopEdge numTrials game env []
   in (toNumber count) / (toNumber numTrials)
  
loopEdge :: Int -> E Boolean -> Experience Unit -> Array Boolean -> Array Boolean
loopEdge 0 _ _   rs = rs
loopEdge n f env rs = 
  let y = f env
  in loopEdge (n-1) f {st: y.st, u: unit} $ y.u : rs

main :: Effect Unit
main = do
  log $ show $ computeHouseEdge 50000 playCrapsOnce 
