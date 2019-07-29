module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.Except(runExcept)
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show(genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (F, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import DOM.Editor as DOM

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
  setup <- DOM.setup
  
  editor <- DOM.createElement "div" setup.document
  
  textArea <- DOM.createElement "textarea" setup.document
  _ <- DOM.setAttribute "cols" "80" textArea
  _ <- DOM.setAttribute "rows" "25" textArea
  _ <- DOM.appendChild textArea editor
 
  decode <- DOM.createElement "div" setup.document
  _ <- DOM.appendChild decode editor
  
  button <- DOM.createElement "button" setup.document
  _ <- DOM.setTextContent "Save" button
  _ <- DOM.appendChild button editor
  
  inputFile <- DOM.createElement "input" setup.document
  _ <- DOM.setAttribute "type" "file" inputFile
  inputListener <- DOM.eventListener (\ev -> do
           DOM.withTextReader (\json -> do
             _ <- DOM.setTextContent json textArea
             _ <- DOM.setTextContent (show $ runExcept (decodeJSON  json :: F {record :: Array {key1 :: Int, key2 :: String}, status :: Boolean})) decode
             pure unit
             ) ev
           
           fileName <- DOM.fileName inputFile
           clickListener <- DOM.eventListener (\_ev -> do
                      suffix <- DOM.dateTimeTag
                      _ <- setup.saveText textArea (suffix <> fileName) _ev
                      _ <- DOM.removeEventListener clickListener DOM.click button
                      pure unit
                  ) 
           _ <- DOM.addEventListener clickListener DOM.click button
           pure unit
       )
  _ <- DOM.addEventListener inputListener DOM.change inputFile
 
  _ <- DOM.appendChild inputFile setup.body
  _ <- DOM.appendChild editor setup.body
  
  logShow $ encodeJSON $ Just {fromJust:[1, 2, 3]}
  logShow $ runExcept (decodeJSON "{\"tag\":\"Just\",\"contents\":{\"fromJust\":[1,2,3]}}" :: F Maybe)  
  logShow $ encodeJSON Nothing
  logShow $ runExcept (decodeJSON "{\"tag\":\"Nothing\"}" :: F Maybe)
