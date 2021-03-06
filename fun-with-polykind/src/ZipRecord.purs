module ZipRecord (zipRecord, ZipProp) where

import Prelude

import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMapWithIndex, class MappingWithIndex, hmapWithIndex)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy)

newtype ZipProp r = ZipProp { | r }

instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProp fns) (Proxy sym) a b where
  mappingWithIndex (ZipProp fns) prop = Record.get prop fns

zipRecord :: forall rfns rin rout.
  HMapWithIndex (ZipProp rfns) { | rin } { | rout } =>
  { | rfns } ->
  { | rin  } ->
  { | rout }
zipRecord =
  hmapWithIndex <<< ZipProp

