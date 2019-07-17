module Example.Random where

import Prelude

import Effect (Effect)
import Effect.Random (random)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasElement, strokePath, fillPath, arc, setStrokeStyle,
                        setFillStyle, getContext2D, getCanvasElementById)
import Math as Math
import Partial.Unsafe (unsafePartial)

foreign import setAttribute :: String -> String -> CanvasElement -> Effect Unit

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  _ <- setAttribute "width" "800" canvas
  _ <- setAttribute "height" "800" canvas
  ctx <- getContext2D canvas

  _ <- setFillStyle ctx "#FF0000" 
  _ <- setStrokeStyle ctx "#000000" 

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , radius: r * 50.0
         , start : 0.0
         , end   : Math.pi * 2.0
         }

    _ <- fillPath ctx path
    strokePath ctx path
