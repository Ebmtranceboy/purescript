module Example.Refs where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.DOM (addEventListener, querySelector)
import Effect.Ref as Ref
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasElement, Context2D, getContext2D, getCanvasElementById,
                        rect, fillPath, translate, scale, rotate, withContext,
                        setFillStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

render :: Context2D -> Int -> Effect Unit
render ctx count = void do
  _ <- setFillStyle ctx "#FFFFFF" 

  _ <- fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , width: 600.0
    , height: 600.0
    }

  _ <- setFillStyle ctx "#00FF00" 

  withContext ctx do
    let scaleX = Math.sin (toNumber count * Math.pi / 4.0) + 1.5
    let scaleY = Math.sin (toNumber count * Math.pi / 6.0) + 1.5

    _ <- translate ctx { translateX: 300.0, translateY:  300.0 }
    _ <- rotate ctx (toNumber count * Math.pi / 18.0) 
    _ <- scale ctx { scaleX: scaleX, scaleY: scaleY }
    _ <- translate ctx { translateX: -100.0, translateY: -100.0 } 

    fillPath ctx $ rect ctx
      { x: 0.0
      , y: 0.0
      , width: 200.0
      , height: 200.0
      }

foreign import setAttribute :: String -> String -> CanvasElement -> Effect Unit

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  _ <- setAttribute "width" "800" canvas
  _ <- setAttribute "height" "800" canvas
  ctx <- getContext2D canvas

  clickCount <- Ref.new 0

  render ctx 0 

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    logShow "Mouse clicked!"
    count <- Ref.modify (\count -> count + 1) clickCount
    render ctx count 
