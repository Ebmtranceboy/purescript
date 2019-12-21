module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Variant(SProxy(..), Variant, case_, inj, on, default)

type RecordRows otherRows =
 ( phase :: Unit
 , voltage :: Unit
 | otherRows
 )

-- no other rows needed, so close the rows with an
-- empty one (i.e. '()')...
type MainRecord = RecordRows ()

-- Refer to type alias for rows.
-- Now we can update our rows at anytime
-- and type signatures don't need updating 
-- (though our value-level code will need to be updated).

g :: Variant MainRecord -> String
g = case_
  # on (SProxy :: SProxy "phase") (const "Dispatch phase...")
  # on (SProxy :: SProxy "voltage") (const "Dispatch voltage...")

h :: Variant MainRecord -> String
h = default "unknown"
  # on (SProxy :: SProxy "phase") (const "Dispatch phase...")
  
somePhase :: forall r. Variant (phase :: Unit |r)
somePhase = inj (SProxy :: SProxy "phase") unit

someVoltage :: forall r. Variant (voltage :: Unit |r)
someVoltage = inj (SProxy :: SProxy "voltage") unit

main :: Effect Unit
main = do
  log $ g somePhase
  log $ g someVoltage
  log $ h somePhase
  log $ h someVoltage
  
