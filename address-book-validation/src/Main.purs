module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Alert (alert)
import Effect.Storage (getItem, setItem)
import Foreign (readNullOrUndefined, readString, renderForeignError)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Web.DOM (Document) as DOM
import Web.DOM.Element (Element, setAttribute, setId, toEventTarget, toNode) as DOM
import Web.DOM.Node (Node, appendChild, setTextContent) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.Event.Event (Event) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.HTML.Event.EventTypes (click) as Event
import Web.HTML (window) as HTML
import Web.HTML.Window (document) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.DOM.Document (createElement, toNonElementParentNode) as DOM
import Web.HTML.HTMLElement (toNode) as HTML
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Partial.Unsafe (unsafePartial)

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  }

derive instance genericFormData :: Generic FormData _

instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

instance showFormData :: Show FormData where
  show = genericShow

loadSavedData :: Effect (Maybe FormData)
loadSavedData = do 
  item <- getItem "person"

  let
      savedData = runExcept do
         json <- traverse readString =<< readNullOrUndefined item
         traverse decodeJSON json

  case savedData of
       Left err -> do
          alert $ "Unable to read saved form data: " <> foldMap (("\n" <> _) <<< renderForeignError) err
          pure Nothing
       Right mdata -> pure mdata

firstInput :: DOM.Document -> Effect DOM.Element
firstInput document = do
  element     <- DOM.createElement "input" document 
  DOM.setAttribute "type" "text" element
  pure element 

lastInput :: DOM.Document -> Effect DOM.Element
lastInput document = do
  element     <- DOM.createElement "input" document 
  DOM.setAttribute "type" "text" element
  pure element 

button :: String -> (Event.Event -> Effect Unit) -> DOM.Document -> Effect DOM.Element
button text onClick document = do
  elem      <- DOM.createElement "button" document 
  _         <- DOM.setTextContent text (DOM.toNode elem)
  let target = DOM.toEventTarget elem
  listener  <- Event.eventListener onClick
  _         <- Event.addEventListener Event.click listener false target
  pure elem

label :: String -> DOM.Document -> Effect DOM.Element
label text document = do
  elem <- DOM.createElement "label" document
  DOM.setTextContent text (DOM.toNode elem)
  pure elem 

controls :: DOM.Node -> DOM.Document -> Effect Unit
controls node document = do
  first       <- firstInput document
  _           <- DOM.setId "first" first
  last        <- lastInput document
  _           <- DOM.setId "last" last
  saveButton  <- button "Save" onClick document
  loadedRec   <- loadSavedData
  name        <- label (show loadedRec) document
  DOM.appendChild (DOM.toNode first) node # void
  DOM.appendChild (DOM.toNode last) node # void
  DOM.appendChild (DOM.toNode saveButton) node # void
  DOM.appendChild (DOM.toNode name) node # void
  
    where

      onClick :: Event.Event -> Effect Unit
      onClick event = do
        first <- DOM.getElementById "first" (DOM.toNonElementParentNode document) 
        firstValue <- traverse HTMLInput.value (HTMLInput.fromElement $ unsafePartial $ fromJust first) 
        last <- DOM.getElementById "last" (DOM.toNonElementParentNode document) 
        lastValue <- traverse HTMLInput.value (HTMLInput.fromElement $ unsafePartial $ fromJust last) 
        let rec = FormData { firstName: unsafePartial $ fromJust firstValue
                     , lastName: unsafePartial $ fromJust lastValue
                     }
        setItem "person" $ encodeJSON rec

main :: Effect Unit
main = void do
  window        <- HTML.window
  htmlDocument  <- HTML.document window
  let document  =  HTML.toDocument htmlDocument
  maybeBody     <- HTML.body htmlDocument
  controls (HTML.toNode $ unsafePartial $ fromJust maybeBody) document

