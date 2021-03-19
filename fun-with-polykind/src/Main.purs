module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Record (delete)
import Data.Foldable (fold)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Type.Proxy (Proxy(..))

import AddSuffixes (addSuffixes)
import RecordApply (recordApply)
import ZipRecord (zipRecord)
import PureValues (pureValues)
import RecordMap (recordMap)
import SequencePropOf (sequencePropOf)
import SequenceRecord (sequenceRecord)
import RcatMaybes (labels, class Predicatable, truc)
import Keys (keys)

recRef :: { a :: Int, b :: String, c :: Boolean }
recRef = { a: 8, b: "z", c: true }

recFun :: { a :: Int -> Number, b :: String -> Int, c :: Boolean -> Boolean }
recFun = { a: toNumber <<< (_ + 4), b: length, c: not }

instance predicInt :: Predicatable (P Int) where
  predicate (P x) = ((_>2) <<< length <<< show ) x
  
instance predicString :: Predicatable (P String) where
  predicate (P x) = ((_>2) <<< length <<< show ) x
  
instance predicBoolean :: Predicatable (P Boolean) where
  predicate (P x) = ((_>2) <<< length <<< show ) x
  
newtype P x = P x

main :: Effect Unit
main = do
  log $ show recRef
  log $ fold $ keys recRef
  log $ show $ delete (Proxy :: Proxy "a") recRef
  log $ show $ recordMap (\x->[x,x]) recRef
  log $ show $ pureValues (\x->[x,x]) recRef
  log $ show $ recordMap Just recRef
  log $ show $ pureValues Just recRef
  log $ show ((sequencePropOf $ pureValues Just recRef) :: Maybe _)
  log $ show ((sequenceRecord $ pureValues Just recRef) :: Maybe _)
  log $ show ((sequencePropOf {d: Just true, e: (Nothing :: Maybe Boolean)}) :: Maybe _)
  log $ show ((sequenceRecord {d: Just true, e: (Nothing :: Maybe Boolean)}) :: Maybe _)
  log $ show ((sequencePropOf {d: 35, e: Just 7}) :: Maybe _)
  log $ show ((sequencePropOf {d: 35, e: [7,9,2]}) :: Array _)
  --log $ show $ rcatMaybes {d: Just 35, e: Just 7, f: Nothing}
  log $ show $ labels $ pureValues P {a:7, b:"",c:true} 
  log $ show truc
  log $ show $ {ac :3,b:identity unit} == {("a"<>"c"):2+1, b:unit}
  log $ show $ addSuffixes (Proxy :: Proxy ".test") recRef
  log $ show $ recordApply recFun recRef
  log $ show $ zipRecord recFun recRef


