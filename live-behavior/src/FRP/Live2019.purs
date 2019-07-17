module FRP.Live2019
  (module FRP.Live2019) where

import Prelude( bind, pure
              , Unit, unit
              , ($), (<$>), (<>), flip, (<<<), (<*>), liftA1
              , not, (*), (/), (-), negate)
import Control.Apply(lift2)
import Effect (Effect)
import Data.Int (toNumber)
import Data.Newtype (unwrap)
import Data.Time.Duration(Seconds(..)) as Duration
import Data.Maybe (fromJust, maybe)
import Data.Set (isEmpty)
import FRP.Behavior (Behavior, animate, integral', fixB, derivative')
import FRP.Behavior.Mouse (buttons, position)
import FRP.Event.Mouse (Mouse, getMouse)
import FRP.Behavior.Time (seconds) as Time
import Graphics.Canvas ( CanvasElement, getCanvasElementById, getContext2D
                       , getCanvasHeight, getCanvasWidth, clearRect)
import Graphics.Drawing (Drawing, render
                        , fillColor, filled, outlined, lineWidth
                        , circle, rectangle)
import Color.Scheme.MaterialDesign (blueGrey, yellow)
import Partial.Unsafe (unsafePartial)

foreign import createCanvas :: Effect CanvasElement

live :: Effect(Behavior Drawing) -> Effect Unit
live eScene = do
  canvas <- createCanvas
  ctx <- getContext2D canvas
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  let background = filled (fillColor blueGrey) (rectangle 0.0 0.0 w h)
  scene <- eScene
  _ <- animate (pure background <> scene) (\frame -> do
    _ <- clearRect ctx { x: 0.0, y: 0.0, width: w, height: h }
    render ctx frame)
  pure unit

by :: forall a b. Effect (Behavior a) -> (a->b) 
                -> Effect (Behavior b)
by = flip $ liftA1 <<< liftA1

-- Bits and pieces

mouse :: Effect (Behavior { x :: Number, y :: Number })
mouse = (position <$> getMouse) `by` (maybe { x: 0.0, y: 0.0 } 
                                            (\{ x, y } -> { x: toNumber x
                                                          , y: toNumber y }))
click :: Effect (Behavior Boolean)
click = (buttons <$> getMouse) `by` (not <<< isEmpty) 

fromSeconds :: Behavior Number
fromSeconds = unwrap <$> Time.seconds

dot :: Number -> Number -> Number -> Drawing
dot x y r = filled   (fillColor yellow)    (circle x y r) 
         <> outlined (lineWidth (r / 4.0)) (circle x y (r * 1.2))

withRadius :: Effect (Behavior Number) -> Effect Unit
withRadius radius = live $ (lift2 $ \{x, y} r -> dot x y r) <$> mouse <*> radius

-- LIVE SESSION

live1 = withRadius $ click `by` (if _ then 100.0 else 50.0)
live2 = withRadius $ (integral' 50.0) <$> pure fromSeconds 
                                      <*> (click `by` (if _ then 50.0 else 0.0))
live3 = withRadius $ do
   bclick <- click
   pure $ fixB 50.0 \x ->  
      integral' 50.0 fromSeconds $ 
        integral' 0.0 fromSeconds $
          (\y dy -> if _ then 100.0 else -5.0 * (y - 50.0) - 2.0 * dy)  
           <$> x  
           <*> derivative' fromSeconds x
           <*> bclick 
