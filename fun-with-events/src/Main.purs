module Main where

import Prelude
import Effect (Effect)
import Control.Alt(alt)
import FRP.Behavior (Behavior, animate, unfold)
import FRP.Event(Event, create)
import FRP.Event.Time(interval)
import Graphics.Canvas (getCanvasElementById, getContext2D
                       , getCanvasHeight, getCanvasWidth
                       , setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, render
                        , rectangle
                        , fillColor, filled)
import Color (rgb)
import Partial.Unsafe (unsafePartial)
import Data.Maybe(fromJust)
import DOM.Editor as DOM

type State = {n :: Number, even :: Boolean}
type ButtonEvent = {event :: Event String, push :: String -> Effect Unit}

scene :: ButtonEvent -> {w::Number, h::Number} -> Behavior Drawing
scene ev {w, h} = map renderState state where
  
  renderState :: State -> Drawing
  renderState {n, even} =
    if even then filled (fillColor $ rgb 123 32 13) (rectangle 10.0 10.0 (10.0*n) (10.0*n)) 
            else filled (fillColor $ rgb 52 42 124) (rectangle 10.0 10.0 (10.0*n) (10.0*n))

  button :: Event String
  button = ev.event

  metro ::  Event String
  metro = (\_ -> "trigger") <$> interval 1000

  state :: Behavior State
  state = unfold (\trigger st -> {n: st.n + (if trigger == "Clicked" then (-5.0) else 1.0), even: not $ st.even}) (metro `alt` button) {n: 10.0, even: true}

cb :: forall a. ButtonEvent -> a -> Effect Unit
cb {event, push} ev = do
  push "Clicked"

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  _ <- setCanvasWidth canvas 320.0
  _ <- setCanvasHeight canvas 512.0
  setup <- DOM.setup
  button <- DOM.createElement "button" setup.document
  _ <- DOM.setTextContent "Click" button
  _ <- DOM.appendChild button setup.body
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  ev <- create
  _ <- DOM.addEventListener (cb ev) DOM.click button
  _ <- animate (scene ev { w, h }) (render ctx)
  pure unit
