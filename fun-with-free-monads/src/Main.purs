module Main where

import Prelude
import Effect (Effect)
--import Effect.Console (logShow)
import Control.Alt(alt)
import FRP.Behavior (Behavior, animate, unfold)
import FRP.Event(Event, create)
--import FRP.Event.Keyboard(down)
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

data My a = Zero | One a | Two a a
                 
instance myShow :: Show a => Show (My a) where
  show Zero = "Zero"
  show (One x) = "One " <> show x
  show (Two x y) = "Two " <> show x <> " " <> show y

instance myFunctor :: Functor My where
  map f (Zero) = Zero
  map f (One x) = Two (f x) (f x)
  map f (Two x y) = One (f y)

data Free f a = Pure a | Roll (f (Free f a))

instance freeShow :: Show a  => Show (Free f a) where
  show (Pure a) = "Pure " <> show a
  show (Roll x) = "Roll"


instance freeFunctor :: Functor f => Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f (Roll x) = Roll (map (map f) x)

concatFree :: forall f a. Functor f => Free f (Free f a) -> Free f a
concatFree (Pure x) = x
concatFree (Roll y) = Roll (map concatFree y)

bindFree :: forall a b f. Functor f => Free f a -> (a -> Free f b) -> Free f b
bindFree x f = concatFree (map f x)

instance freeApply :: ( Functor f)  => Apply (Free f) where
  apply ff fx =  bindFree fx (\x -> bindFree ff (\f -> Pure $ f x)) 
   
instance freeApplicative :: Functor f => Applicative (Free f) where
  pure = Pure

instance freeBind :: Functor f => Bind (Free f) where
  bind x f = bindFree x f 

type MyMonad a = Free My a

liftFree :: forall a f. Functor f => f a -> Free f a
liftFree x = Roll (map Pure x)

foldFree :: forall f r. Functor f => (f r -> r) -> Free f r -> r
foldFree _ (Pure a) = a
foldFree f (Roll x) = f (map (foldFree f) x)

extract :: forall a. Monoid a => My a -> a
extract Zero = mempty
extract (One x) = x
extract (Two x y) = x <> y

test0 :: MyMonad String
test0 = do
  n <- (\x -> x <> x) <$> (liftFree $ One "A")
  pure n

test1 :: MyMonad (Array Int)
test1 = do
  n <- (\x -> x <> x) <$> (liftFree $ Two [12] [5])
  pure n

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
  
{-
main :: Effect Unit
main = do
  logShow $ One 2
  logShow $ (_+4) <$> One 2
  logShow $ foldFree extract test0
  logShow $ foldFree extract test1

 -} 

