module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.Except(runExcept)
import Data.Generic.Rep(class Generic)
import Data.Generic.Rep.Show(genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (F, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode) as Foreign
import DOM.Editor as DOM
import Global.Unsafe(unsafeEncodeURIComponent)
import Data.Argonaut
import Data.Either

data Maybe = Nothing
  | Just { fromJust  :: Array Int}

derive instance genericMaybe :: Generic Maybe _

instance decodeMaybe :: Decode Maybe where
  decode = Foreign.genericDecode (Foreign.defaultOptions)

instance encodeMaybe :: Encode Maybe where
  encode = Foreign.genericEncode (Foreign.defaultOptions )

instance showMaybe :: Show Maybe where
  show = genericShow

save :: DOM.Node -> String -> {window :: DOM.Window, document :: DOM.Document, body :: DOM.Node} -> DOM.Event -> Effect Unit
save textNode fileName setup ev = do
  text <- DOM.value textNode
  pom <- DOM.createElement "a" setup.document
  _ <- DOM.setAttribute "href" ("data:text/plain;charset=utf-8," <> unsafeEncodeURIComponent text) pom
  _ <- DOM.setAttribute "download" fileName pom
  _ <- DOM.setAttribute "style" "display=\"none\"" pom
  _ <- DOM.appendChild pom setup.body
  _ <- DOM.doClick pom
  _ <- DOM.removeChild pom
  pure unit

main :: Effect Unit
main = do
  setup <- DOM.setup
  suffix <- DOM.dateTimeTag
  
  editor <- DOM.createElement "div" setup.document
  
  textArea <- DOM.createElement "textarea" setup.document
  _ <- DOM.setAttribute "cols" "80" textArea
  _ <- DOM.setAttribute "rows" "25" textArea
  _ <- DOM.appendChild textArea editor
 
  decode <- DOM.createElement "div" setup.document
  _ <- DOM.appendChild decode editor
  
  inputFile <- DOM.createElement "input" setup.document
  _ <- DOM.setAttribute "type" "file" inputFile
  _ <- DOM.addEventListener (\ev -> do
           DOM.withTextReader (\json -> do
             _ <- DOM.setTextContent json textArea
             _ <- DOM.setTextContent (show $ runExcept (Foreign.decodeJSON  json :: Foreign.F {record :: Array {key1 :: Int, key2 :: String}, status :: Boolean})) decode
             pure unit
             ) ev
           
           button <- DOM.createElement "button" setup.document
           _ <- DOM.setTextContent "Save" button
           fileName <- DOM.fileName inputFile
           _ <- DOM.addEventListener (save textArea (suffix <> fileName) setup) DOM.click button
           _ <- DOM.appendChild button editor
           pure unit
 
       ) DOM.change inputFile

  _ <- DOM.appendChild inputFile setup.body
  _ <- DOM.appendChild editor setup.body
  
  logShow $ runExcept (Foreign.decodeJSON "\"Testing\"" :: Foreign.F String)
  logShow $ runExcept (Foreign.decodeJSON "true" :: Foreign.F Boolean)
  logShow $ runExcept (Foreign.decodeJSON "{\"a\":true, \n \n \"b\":6, \"c\":\"zk\"}" :: Foreign.F {a::Boolean, b::Int,c::String})
  logShow $ runExcept (Foreign.decodeJSON "[1, 2, 3]" :: Foreign.F (Array Int))
  logShow $ Foreign.encodeJSON $ Just {fromJust:[1, 2, 3]}
  logShow $ runExcept (Foreign.decodeJSON "{\"tag\":\"Just\",\"contents\":{\"fromJust\":[1,2,3]}}" :: Foreign.F Maybe)  
  logShow $ Foreign.encodeJSON Nothing
  logShow $ runExcept (Foreign.decodeJSON "{\"tag\":\"Nothing\"}" :: Foreign.F Maybe)
