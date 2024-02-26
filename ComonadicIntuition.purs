module Main where

import Prelude
import Effect (Effect)

{-

The object intuition
--------------------
In order to incrementally build an object defined by a set of attributes, we need

* a model :
newtype Object s p = Object (s p)

* a smart constructor :
makObject :: s p -> Object s p
makeObject ps = Object ps

* a builder with default attributes :
withDefault :: s p -> Object s p
withDefault ps = Object (ds <> ps)

* a custummized builder :
profile :: (s p -> Object s p) -> Object s p
profile attrDef = attrDef attrCust

But, once prof = profile withDefault is set, it is no longer editable anymore.
Instead, we need a way to extend profile so that it can take futher sets of attributes
extend profile :: (s p -> Object s p) -> s p -> Object s p
extend ::  ((s p -> Object s p) -> Object s p) -> (s p -> Object s p) -> s p -> Object s p
type A = Object s p
type B = Object s p
type C x = s p -> x
extend :: (C A -> B) -> C A -> C B

Then, when needed, the object is extracted from the extended chain of profiles :
extract :: (s p -> Object s p) -> Object s p
extract :: C A -> A

The two operations should be as neutral as possible :

extract (extend profile someFunctionality) = profile someFunctionality
extend extract someFunctionality = someFunctionality
extend (prof1 =>= prof2) = prof1 =>= extend prof2

with the intuition that extending a composition (=>=) of profiles 
has the same effect as extending the last one.

-}

main :: Effect Unit
main = pure unit
