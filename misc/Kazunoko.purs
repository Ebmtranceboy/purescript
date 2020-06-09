module Kazunoko where

import Prim.TypeError (class Fail, Text)
import Type.Prelude (SProxy(..))

class Succ (i :: Symbol) (o :: Symbol) | i -> o, o -> i
instance zeroSucc :: Succ "zero" "one"
else instance oneSucc :: Succ "one" "two"
else instance twoSucc :: Succ "two" "three"
else instance threeSucc :: Succ "three" "four"
else instance fourSucc :: Succ "four" "five"
else instance fiveSucc :: Succ "five" "six"
else instance sixSucc :: Succ "six" "seven"
else instance sevenSucc :: Succ "seven" "eight"
else instance eightSucc :: Succ "eight" "nine"
else instance nineSucc :: Succ "nine" "ten"
else instance noSucc ::
  ( Fail (Text "i don't know how to count any bigger than ten or less than zero")
  ) => Succ i o

class Add (l :: Symbol) (r :: Symbol) (o :: Symbol) | l -> r o
instance zeroAdd :: Add "zero" r r
else instance succAdd ::
  ( Succ l' l
  , Succ r r'
  , Add l' r' o
  ) => Add l r o

add
  :: forall l r o
   . Add l r o
  => SProxy l
  -> SProxy r
  -> SProxy o
add _ _ = SProxy

class Sub (l :: Symbol) (r :: Symbol) (o :: Symbol) | r -> l o
instance zeroSub :: Sub l "zero" l
else instance succSub ::
  ( Succ l' l
  , Succ r' r
  , Sub l' r' o
  ) => Sub l r o

subtract
  :: forall l r o
   . Sub l r o
  => SProxy l
  -> SProxy r
  -> SProxy o
subtract _ _ = SProxy
