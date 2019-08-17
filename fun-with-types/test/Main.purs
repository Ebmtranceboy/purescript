module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)
import Type.Prelude (SProxy(..), class Append, reifySymbol, reflectSymbol,class IsSymbol)
import Type.Prelude (append) as Type
import Prim.Symbol(class Cons) as Symbol
import Record(rename)
import Data.String(joinWith)
import ExtractKeys(keys)

-- REFLECT INT

class ReflectInt (s :: Symbol) where
  reflectInt :: SProxy s -> Int

instance reflectIntZero :: ReflectInt "" where
  reflectInt _ = 0

else instance reflectIntN ::
  ( Symbol.Cons "." m n
  , ReflectInt m
  ) => ReflectInt n where
  reflectInt _ = 1 + (reflectInt (SProxy :: SProxy m))

-- PEANO UNNECESSITY

type Zero
  = ""
type Two
  = ".."
type Five
  = "....."
type Seven
  = "......."

minus :: forall l r o. Append r o l => SProxy l -> SProxy r -> SProxy o
minus _ _ = SProxy

-- resultSub :: SProxy ".."
resultSub :: SProxy Two
resultSub = 
  minus (SProxy :: SProxy Seven) (SProxy :: SProxy Five)

resultSub2 :: SProxy "..."
resultSub2 =
  minus (SProxy :: SProxy Five) (SProxy :: SProxy Two)

-- RECORD 101

type Rep = "bora"
sprep = SProxy :: SProxy Rep

record = {ab: 8, cde: true}  :: {ab :: Int, cde :: Boolean}

-- WHAT OTHER USE OF SYMBOLS

newtype Sep (s :: Symbol) = Sep (Array String)

sep :: forall s. String -> Sep s
sep s = Sep [s]

derive newtype instance semigroupSep :: Semigroup (Sep s)
derive newtype instance monoidSep :: Monoid (Sep s)

renderSep :: forall s. IsSymbol s => Sep s -> String
renderSep (Sep items) = let sep' = reflectSymbol (SProxy :: SProxy s)
                        in joinWith sep' items

comeFrom :: forall s. IsSymbol s => SProxy s -> String
comeFrom _ = renderSep (sep "I come from " <> sep "!!!" :: Sep s)

main :: Effect Unit
main = do
  assert $ reflectInt (SProxy :: SProxy Zero) == 0
  assert $ reflectInt (SProxy :: SProxy "") == 0
  assert $ reflectSymbol (SProxy :: SProxy Two) == ".."
  assert $ reflectInt resultSub == 2
  assert $ reflectInt resultSub2 == 3
  assert $ reflectInt (Type.append (SProxy :: SProxy Five) (SProxy :: SProxy Seven)) == 12
  assert $ reflectInt (SProxy :: forall o. Append Five Seven o => SProxy o) == 12
  assert $ reflectInt (SProxy :: forall o. Append Five o Seven => SProxy o) == 2
  assert $ keys record == ["ab", "cde"]
  assert $ (rename (SProxy :: SProxy "ab") (Type.append sprep sprep) record).borabora == 8
  assert $  reflectSymbol sprep == "bora"
  log $ reifySymbol "runtime" comeFrom
  log $ comeFrom (SProxy :: SProxy "compile")
  assert $ reflectSymbol (SProxy :: forall h. Symbol.Cons h "ead" "head" => SProxy h) == "h" 
  assert $ reflectSymbol (SProxy :: forall ail. Symbol.Cons "t" ail "tail" => SProxy ail) == "ail" 
  assert $ reflectSymbol (SProxy :: forall cons. Symbol.Cons "c" "ons" cons => SProxy cons) == "cons" 
  
