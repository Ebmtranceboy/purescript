module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Record (delete)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Type.Proxy (Proxy(..))

import AddSuffixes (addSuffixes)
import RecordApply (recordApply)
import ZipRecord (zipRecord)
import PureValues (pureValues)
import RecordMap (recordMap)

recRef :: { a :: Int, b :: String, c :: Boolean }
recRef = { a: 8, b: "z", c: true }

recFun :: { a :: Int -> Number, b :: String -> Int, c :: Boolean -> Boolean }
recFun = { a: toNumber <<< (_ + 4), b: length, c: not }

main :: Effect Unit
main = do
  log $ show recRef
  log $ show $ pureValues Just recRef
  log $ show $ pureValues (\x->[x,x]) recRef
  log $ show $ recordMap Just recRef
  log $ show $ recordMap (\x->[x,x]) recRef
  log $ show $ addSuffixes (Proxy :: Proxy ".test") recRef
  log $ show $ delete (Proxy :: Proxy "a") recRef
  log $ show $ recordApply recFun recRef
  log $ show $ zipRecord recFun recRef
 
