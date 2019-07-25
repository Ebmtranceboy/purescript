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
import Control.Monad.Maybe.Trans (lift, MaybeT(MaybeT), runMaybeT)
import Effect.Aff (runAff)
import Effect.Class (liftEffect)
import Web.File.File (File, toBlob)
import Web.File.FileList (item)
import Web.File.FileReader.Aff (readAsArrayBuffer, readAsText)
import Web.HTML.HTMLInputElement (HTMLInputElement, files, fromNode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

data Maybe = Nothing
  | Just { fromJust  :: Array Int}

derive instance genericMaybe :: Generic Maybe _

instance decodeMaybe :: Decode Maybe where
  decode = genericDecode (defaultOptions)

instance encodeMaybe :: Encode Maybe where
  encode = genericEncode (defaultOptions )

instance showMaybe :: Show Maybe where
  show = genericShow

changedInput node area event = do
  let reader :: File -> Effect Unit
      reader f = void $ runAff (\_ -> pure unit) do
        res <- readAsText (toBlob f)
        liftEffect $ DOM.setTextContent res area
  void $ runMaybeT do
    fs <- MaybeT $ files node
    file <- MaybeT $ pure $ item 0 fs
    lift $ reader file
  pure unit


main :: Effect Unit
main = do
  setup <- DOM.setup
  
  textArea <- DOM.createElement "textarea" setup.document
  _ <- DOM.setAttribute "cols" "80" textArea
  _ <- DOM.setAttribute "rows" "25" textArea
  
  inputFile <- DOM.createElement "input" setup.document
  _ <- DOM.setAttribute "type" "file" inputFile
  _ <- DOM.addEventListener (changedInput (unsafePartial $ fromJust $ fromNode inputFile) textArea) DOM.change inputFile

  _ <- DOM.appendChild inputFile setup.body
  _ <- DOM.appendChild textArea setup.body
  
  logShow $ runExcept (decodeJSON "\"Testing\"" :: F String)
  logShow $ runExcept (decodeJSON "true" :: F Boolean)
  logShow $ runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
  logShow $ encodeJSON $ Just {fromJust:[1, 2, 3]}
  logShow $ runExcept (decodeJSON "{\"tag\":\"Just\",\"contents\":{\"fromJust\":[1,2,3]}}" :: F Maybe)  
  logShow $ encodeJSON Nothing
  logShow $ runExcept (decodeJSON "{\"tag\":\"Nothing\"}" :: F Maybe)
