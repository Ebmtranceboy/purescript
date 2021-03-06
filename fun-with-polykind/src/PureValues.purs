module PureValues (pureValues, PureMember) where

import Prelude

import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy)

data PureMember (f :: Type -> Type) = PureMember (forall a. a -> f a)

instance pureMember ::
  Mapping (PureMember f) a (f a) where
    mapping (PureMember f) a = f a
    
pureValues ::
  forall f r1 r2.
  HMap (PureMember f) r1 r2 =>
  (forall a. a -> f a) ->
  r1 ->
  r2
pureValues f = hmap (PureMember f)
