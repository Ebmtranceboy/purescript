module Main where

import Prelude
import Effect (Effect)
import DOM.Editor as DOM

cb :: forall a. DOM.Document -> a -> Effect Unit
cb doc event = do
  p2 <- DOM.getElementById "P2" doc
  input <- DOM.getElementById "input" doc
  str <- DOM.inputValue input
  _ <- DOM.setTextContent str p2
  _ <- DOM.removeChild input
  pure unit

main :: Effect Unit
main = do
  setup <- DOM.setup
  p1 <- DOM.createElement "p" setup.document
  _ <- DOM.setTextContent "HelloWorld" p1
  _ <- DOM.setAttribute "id" "P1" p1
  _ <- DOM.appendChild p1 setup.body
  p2 <- DOM.createElement "p" setup.document
  _ <- DOM.setTextContent "HelloWorld" p2
  _ <- DOM.setId "P2" p2
  _ <- DOM.appendChild p2 setup.body
  pOne <- DOM.getElementById "P1" setup.document
  _ <- DOM.setTextContent "Hello, world!!" pOne
  input <- DOM.createElement "input" setup.document
  _ <- DOM.setAttribute "id" "input" input
  _ <- DOM.appendChild input setup.body
  button <- DOM.createElement "button" setup.document
  _ <- DOM.setTextContent "Click Me" button
  _ <- DOM.addEventListener (cb setup.document) DOM.click button
  _ <- DOM.appendChild button setup.body
  pure unit
