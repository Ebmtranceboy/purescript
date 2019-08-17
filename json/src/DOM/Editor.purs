module DOM.Editor(module DOM.Editor) where

import Prelude
import Data.Traversable(traverse)
import Web.DOM.Element ( Element, setAttribute, setId
                       , toEventTarget, toNode, fromNode) as DOM
import Web.DOM.Node ( Node, appendChild, parentNode, removeChild
                    , setTextContent, fromEventTarget, textContent) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.Event.Event (Event,EventType,target) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.Event.Internal.Types(EventTarget) as Event
import Web.HTML.Event.EventTypes (click,change) as Event
import Web.HTML (Window, window) as HTML
import Web.HTML.Window (document) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.DOM.Document ( Document, createElement
                        , toNonElementParentNode) as DOM
import Web.HTML.HTMLElement (toNode, click, fromNode) as HTML
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Effect(Effect)
import Control.Monad.Maybe.Trans (lift, MaybeT(MaybeT), runMaybeT)
import Effect.Aff (runAff)
import Effect.Class (liftEffect)
import Web.File.File (File, toBlob, name)
import Web.File.FileList (item)
import Web.File.FileReader.Aff (readAsText)
import Web.HTML.HTMLInputElement (files, fromEventTarget, fromNode) as Element
import Web.HTML.HTMLTextAreaElement(fromNode, value) as TextArea 
import Effect.Now(nowDateTime)
import Data.DateTime(date, day, hour, millisecond, minute, month, second, time, year)
import Data.Foldable(foldr)
import Data.Enum(fromEnum)
import Global.Unsafe(unsafeEncodeURIComponent)

type Document = DOM.Document
type Node = DOM.Node
type Event = Event.Event
type Window = HTML.Window

setup :: Effect { window :: HTML.Window
                , document :: DOM.Document
                , body :: DOM.Node
                , saveText :: DOM.Node -> String -> Event.Event -> Effect Unit}
setup = do
  window        <- HTML.window
  htmlDocument  <- HTML.document window
  let document  =  HTML.toDocument htmlDocument
  maybeBody     <- HTML.body htmlDocument
  let bodyRaw = unsafePartial $ fromJust maybeBody
  let body = HTML.toNode bodyRaw
  pom <- createElement "a" document
  _ <- appendChild pom body
  _ <- setAttribute "style" "display=\"none\"" pom
  let saveText textNode nameOfFile _ = do
        text <- value textNode
        _ <- setAttribute "href" ("data:text/plain;charset=utf-8," <> unsafeEncodeURIComponent text) pom
        _ <- setAttribute "download" nameOfFile pom
        _ <- doClick pom
        pure unit

  pure {window, document, body, saveText}

createElement :: String -> DOM.Document -> Effect DOM.Node
createElement str doc = DOM.toNode <$> DOM.createElement str doc

appendChild :: DOM.Node -> DOM.Node -> Effect DOM.Node
appendChild = DOM.appendChild

removeChild :: DOM.Node -> Effect Unit
removeChild node = do
  parent <- DOM.parentNode node
  _ <- DOM.removeChild node $ unsafePartial $ fromJust parent
  pure unit

toNode :: DOM.Element -> DOM.Node
toNode = DOM.toNode

setTextContent :: String -> DOM.Node -> Effect Unit
setTextContent = DOM.setTextContent

inputValue :: DOM.Node  -> Effect String 
inputValue input = do
  val <- traverse HTMLInput.value 
                  (HTMLInput.fromElement $ unsafePartial 
                                         $ fromJust $ DOM.fromNode input)
  pure $ unsafePartial $ fromJust val

setAttribute :: String -> String -> DOM.Node -> Effect Unit
setAttribute key val node = 
  DOM.setAttribute key val (unsafePartial $ fromJust $ DOM.fromNode node)

setId :: String -> DOM.Node -> Effect Unit
setId id node = DOM.setId id (unsafePartial $ fromJust $ DOM.fromNode node)

getElementById :: String -> DOM.Document -> Effect DOM.Node
getElementById str doc = 
  (\x -> DOM.toNode $ unsafePartial $ fromJust x) 
    <$> DOM.getElementById str (DOM.toNonElementParentNode doc)

addEventListener :: (Event.Event -> Effect Unit)
                 -> Event.EventType 
                 -> DOM.Node                  -> Effect Unit 
addEventListener cb ev node = do
  listener <- (Event.eventListener cb)
  Event.addEventListener 
           ev 
           listener
           false 
           (DOM.toEventTarget (unsafePartial $ fromJust 
                                             $ DOM.fromNode node))

doClick :: DOM.Node -> Effect Unit
doClick node = HTML.click (unsafePartial $ fromJust $ HTML.fromNode node)

click :: Event.EventType
click = Event.click

change :: Event.EventType
change = Event.change

fromEventTarget :: Event.EventTarget -> DOM.Node
fromEventTarget target = 
  unsafePartial $ fromJust $ DOM.fromEventTarget target

textContent :: DOM.Node -> Effect String
textContent node = DOM.textContent node

withTextReader :: (String -> Effect Unit) 
               -> Event.Event          -> Effect Unit
withTextReader whatToDo ev = do
  let reader :: File -> Effect Unit
      reader f = void $ runAff (\_ -> pure unit) do
         str <- readAsText (toBlob f)
         liftEffect $ whatToDo str
  let target = unsafePartial $ fromJust $ Event.target ev
  void $ runMaybeT do
    fs <- MaybeT $ Element.files (unsafePartial 
                 $ fromJust 
                 $ Element.fromEventTarget target)
    file <- MaybeT $ pure $ item 0 fs
    lift $ reader file
  pure unit

fileName :: DOM.Node -> Effect String
fileName node =  do
     fs <- Element.files (unsafePartial
                  $ fromJust
                  $ Element.fromNode node)
     let file = unsafePartial $ fromJust $ item 0 $ unsafePartial $ fromJust fs
     pure $ name file

value :: DOM.Node -> Effect String
value node = TextArea.value $ unsafePartial $ fromJust $ TextArea.fromNode node

dateTimeTag :: Effect String
dateTimeTag = do
  now <- nowDateTime
  let d = date now
  let t = time now
  pure $ (foldr (<>) "" $ map (\x-> x <> "-") 
    [ show $ fromEnum $ day d
    , show $ month d
    , show $ fromEnum $ year d
    , show $ fromEnum $ hour t 
    , show $ fromEnum $ minute t
    , show $ fromEnum $ second t]) 
       <> (show $ fromEnum $ millisecond t)

