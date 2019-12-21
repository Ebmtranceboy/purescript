module Main where
-- Oscillator nodes and gain nodes.

import Prelude

import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext (createGain, createOscillator, currentTime, destination, newAudioContext, resume, state, suspend)
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator (OscillatorType(..), frequency, setOscillatorType, startOscillator)
import Audio.WebAudio.Types (AudioContext, GainNode, OscillatorNode, AudioContextState(..), connect)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref, new, read, write)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

beep
  :: AudioContext
  -> OscillatorNode
  -> GainNode
  -> Effect Unit
beep ctx osc g = do
  freqParam <- frequency osc
  f <- getValue freqParam
  setValue (if f == 55.0 then 53.0 else 55.0) freqParam

  t <- currentTime ctx
  gainParam <- gain g
  _ <- setValueAtTime 0.5 t gainParam
  _ <- setValueAtTime 0.001 (t + 0.2) gainParam
  pure unit

controls
  :: Ref IntervalId
  -> AudioContext
  -> OscillatorNode
  -> GainNode
  -> Effect Unit
controls ref ctx osc g = do
  s <- state ctx
  if (s == SUSPENDED)
     then do
       resume ctx
       t <- setInterval 1000 $ beep ctx osc g
       write t ref
     else do
       suspend ctx
       val <- read ref
       clearInterval val

main :: Effect Unit
main = do
  ctx <- newAudioContext

  osc <- createOscillator ctx
  setOscillatorType Square osc
  startOscillator 0.0 osc

  g <- createGain ctx
  setValue 0.0 =<< gain g

  connect osc g
  connect g =<< destination ctx

  suspend ctx

  let id = unsafeCoerce (new 0) :: Ref IntervalId

  doc <- map toParentNode (window >>= document)
  play <- querySelector (wrap "#play") doc
  case play of
    Just e -> do
      el <- eventListener \_ -> controls id ctx osc g
      addEventListener (wrap "click") el false (unsafeCoerce e :: EventTarget)
    Nothing -> throw "No 'play' button"
  stop <- querySelector (wrap "#stop") doc
  case stop of
    Just e -> do
      el  ← eventListener \_ -> controls id ctx osc g
      addEventListener (wrap "click") el false (unsafeCoerce e :: EventTarget)
    Nothing -> throw "No 'stop' button"
  pure unit
