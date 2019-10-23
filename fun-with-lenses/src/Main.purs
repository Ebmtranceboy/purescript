module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Lens (_2, _Just, _Left, over, preview, is)
import Data.Lens.Index (ix)
import Data.Lens.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Traversal (Traversal')
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Type.Prelude (SProxy(..))


type NestedData =
  Maybe (Array { foo :: Tuple String (Either Int Boolean), bar :: String })

_foo :: forall a r. Lens' { foo :: a | r } a
_foo = prop (SProxy :: SProxy "foo")

_NestedInt :: Int -> Traversal' NestedData Int
_NestedInt index = _Just <<< ix index <<< _foo <<< _2 <<< _Left

getNestedInt :: Int -> NestedData -> Maybe Int
getNestedInt index = preview (_NestedInt index)

modifyNestedInt :: Int -> (Int -> Int) -> NestedData -> NestedData
modifyNestedInt index = over (_NestedInt index)

x :: NestedData
x = Just [ {foo: Tuple "hey" (Left 3), bar: "you"}
         , {foo: Tuple "sun" (Right true), bar: "ten"}
         , {foo: Tuple "min" (Left 9), bar: "cat"}]

type System = {u :: Number, r :: Number, i :: Number}

system :: System
system = {u: 6.0, r: 2.0, i: 3.0}

_u :: forall a r. Lens' { u :: a | r } a
_u = prop (SProxy :: SProxy "u")

main :: Effect Unit
main = do
  log $ show $ getNestedInt 2 x
  log $ show $ modifyNestedInt 0 (_ * 4) x
 -- log $ show $ is _u {r: 7.2}
  log $ show $ preview _u system
