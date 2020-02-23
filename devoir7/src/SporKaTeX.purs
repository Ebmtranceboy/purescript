module SporKaTeX ( fromIncremental
                 , render
                 , put
                 , get
                 , RenderEffect(..)
                 , runRenderEffect
                 , t
                 , mathInline
                 , nl
                 , em
                 , b
                 , setTitle
                 , section
                 , subsection
                 , subsubsection
                 , mathEquation) where

import Prelude

import Effect (Effect)
import Data.Foldable(for_)
import Data.Array(snoc)
import Data.Tuple(fst)

import Control.Monad.State(State, runState)
import Control.Monad.State.Class(class MonadState, modify)
import Control.Monad.State.Class(get) as Control

import Web.DOM.Element (Element) as DOM
import Web.HTML.HTMLLabelElement(HTMLLabelElement, fromElement)

import Spork.Html as H
 
get :: forall m s. MonadState s m => m s
get = Control.get

foreign import render :: String -> HTMLLabelElement -> Effect Unit
foreign import renderToString :: String -> Effect String

put :: forall a st. Functor st => MonadState (Array a) st => a -> st Unit
put x = void $ modify (flip snoc x)

fromIncremental :: forall a. State (Array a) (Array a) -> Array a
fromIncremental seq = fst $ runState seq []

data RenderEffect a
  = RenderEffect String DOM.Element a

runRenderEffect ∷ RenderEffect ~> Effect
runRenderEffect (RenderEffect str el next) = do
    for_ (fromElement el) $ 
        render str     
    pure next

type IncrementalArrayLine a = State (Array (H.Html a)) Unit

t :: forall a. String -> IncrementalArrayLine a 
t str = put $ H.label [] [H.text str]

mathInline :: forall a. (String -> H.ElementRef -> a) -> String -> IncrementalArrayLine a
mathInline renderElement str = put $ H.label [H.ref (H.always (renderElement str))] []

nl :: forall a. IncrementalArrayLine a
nl = put $ H.br []

em :: forall a. String -> IncrementalArrayLine a 
em str = put $ H.em [] [H.text str]

b :: forall a. String -> IncrementalArrayLine a 
b str = put $ H.b [] [H.text str]

setTitle :: forall a. String -> IncrementalArrayLine a 
setTitle str = put $ H.h1 [] [H.text str]

section :: forall a. String -> IncrementalArrayLine a 
section str = put $ H.h2 [] [H.text str]

subsection :: forall a. String -> IncrementalArrayLine a 
subsection str = put $ H.h3 [] [H.text str]

subsubsection :: forall a. String -> IncrementalArrayLine a 
subsubsection str = put $ H.h4 [] [H.text str]

mathEquation :: forall a. (String -> H.ElementRef -> a) -> String -> IncrementalArrayLine a 
mathEquation renderElement str = put $ 
                      H.label [H.style "display: block; text-align: center;"
                              ,H.ref (H.always (renderElement str))] 
                                   [H.text str]
