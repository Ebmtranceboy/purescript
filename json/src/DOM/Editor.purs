module DOM.Editor(module DOM.Editor) where

import Prelude
import Data.Traversable(traverse)
import Web.DOM.Element (Element, setAttribute, setId, toEventTarget, toNode, fromNode) as DOM
import Web.DOM.Node (Node, appendChild, parentNode, removeChild, setTextContent, fromEventTarget, textContent) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.Event.Event (Event,EventType) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.Event.Internal.Types(EventTarget) as Event
import Web.HTML.Event.EventTypes (click,change) as Event
import Web.HTML (Window, window) as HTML
import Web.HTML.Window (document) as HTML
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.DOM.Document (Document, createElement, toNonElementParentNode) as DOM
import Web.HTML.HTMLElement (toNode) as HTML
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Effect(Effect)

type Document = DOM.Document

setup :: Effect {window :: HTML.Window, document :: DOM.Document, body :: DOM.Node}
setup = do
  window        <- HTML.window
  htmlDocument  <- HTML.document window
  let document  =  HTML.toDocument htmlDocument
  maybeBody     <- HTML.body htmlDocument
  let bodyRaw = unsafePartial $ fromJust maybeBody
  let body = HTML.toNode bodyRaw
  pure {window, document, body}

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
  val <- traverse HTMLInput.value (HTMLInput.fromElement $ unsafePartial $ fromJust $ DOM.fromNode input)
  pure $ unsafePartial $ fromJust val

setAttribute :: String -> String -> DOM.Node -> Effect Unit
setAttribute key val node = DOM.setAttribute key val (unsafePartial $ fromJust $ DOM.fromNode node)

setId :: String -> DOM.Node -> Effect Unit
setId id node = DOM.setId id (unsafePartial $ fromJust $ DOM.fromNode node)

getElementById :: String -> DOM.Document -> Effect DOM.Node
getElementById str doc = (\x -> DOM.toNode $ unsafePartial $ fromJust x) <$> DOM.getElementById str (DOM.toNonElementParentNode doc)

addEventListener :: (Event.Event -> Effect Unit) -> Event.EventType -> DOM.Node -> Effect Unit
addEventListener cb ev node = do
  listener <- Event.eventListener cb
  _ <- Event.addEventListener ev listener false (DOM.toEventTarget (unsafePartial $ fromJust $ DOM.fromNode node))
  pure unit

click :: Event.EventType
click = Event.click

change :: Event.EventType
change = Event.change

fromEventTarget :: Event.EventTarget -> DOM.Node
fromEventTarget target = unsafePartial $ fromJust $ DOM.fromEventTarget target

textContent :: DOM.Node -> Effect String
textContent node = DOM.textContent node
