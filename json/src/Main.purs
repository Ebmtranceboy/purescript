module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.Except(runExcept)
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show(genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (F, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)

data Maybe = Nothing
  | Just { fromJust  :: Array Int}

derive instance genericMaybe :: Generic Maybe _

instance decodeMaybe :: Decode Maybe where
  decode = genericDecode (defaultOptions)

instance encodeMaybe :: Encode Maybe where
  encode = genericEncode (defaultOptions )

instance showMaybe :: Show Maybe where
  show = genericShow

main :: Effect Unit
main = do
  logShow $ runExcept (decodeJSON "\"Testing\"" :: F String)
  logShow $ runExcept (decodeJSON "true" :: F Boolean)
  logShow $ runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
  logShow $ encodeJSON $ Just {fromJust:[1, 2, 3]}
  logShow $ runExcept (decodeJSON "{\"tag\":\"Just\",\"contents\":{\"fromJust\":[1,2,3]}}" :: F Maybe)  
  logShow $ encodeJSON Nothing
  logShow $ runExcept (decodeJSON "{\"tag\":\"Nothing\"}" :: F Maybe)
