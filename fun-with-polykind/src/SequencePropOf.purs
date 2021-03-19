module SequencePropOf (sequencePropOf, SequencePropOf) where

import Prelude

import Data.Symbol (class IsSymbol)
import Heterogeneous.Folding (class HFoldlWithIndex, class FoldingWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks) as Row
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy)

data SequencePropOf (f :: Type -> Type) = SequencePropOf

instance sequencePropOf_1 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym a rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (Proxy sym)
    (f (Builder { | ra } { | rb }))
    (f a)
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex _ prop rin a =
    (>>>) <$> rin <*> (Builder.insert prop <$> a)
else
instance sequencePropOf_2 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (Proxy sym)
    (f (Builder { | ra } { | rb }))
    x
    (f (Builder { | ra } { | rc }))
  where
  foldingWithIndex _ prop rin x =
    (_ >>> Builder.insert prop x) <$> rin

sequencePropOf :: forall f rin rout.
  Applicative f =>
  HFoldlWithIndex (SequencePropOf f) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  { | rin } ->
  f { | rout }
sequencePropOf =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (SequencePropOf :: SequencePropOf f) (pure identity :: f (Builder {} {}))

