module SequenceRecord (sequenceRecord, class SequenceRecord, sequenceRecordImpl) where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList as RL
import Record (get) as R
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

class SequenceRecord :: forall k. k -> Row Type -> Row Type -> Row Type -> (Type -> Type) -> Constraint
class Functor m <= SequenceRecord rl row from to m
  | rl -> row from to m
  where
    sequenceRecordImpl :: Proxy rl -> Record row -> m (Builder { | from } { | to })

instance sequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Functor m
  , Row.Lacks name ()
  , Row.Cons name ty () to
  ) => SequenceRecord (RL.Cons name (m ty) RL.Nil) row () to m where
  sequenceRecordImpl _ a  =
       Builder.insert namep <$> valA
    where
      namep = Proxy :: Proxy name
      valA = R.get namep a

else instance sequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Apply m
  , SequenceRecord tail row from from' m
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => SequenceRecord (RL.Cons name (m ty) tail) row from to m where
  sequenceRecordImpl _ a  =
       fn <$> valA <*> rest
    where
      namep = Proxy :: Proxy name
      valA = R.get namep a
      tailp = Proxy :: Proxy tail
      rest = sequenceRecordImpl tailp a
      fn valA' rest' = Builder.insert namep valA' <<< rest'

instance sequenceRecordNil :: Applicative m => SequenceRecord RL.Nil row () () m where
  sequenceRecordImpl _ _ = pure identity

sequenceRecord :: forall row row' rl m
   . RL.RowToList row rl
  => SequenceRecord rl row () row' m
  => Record row
  -> m (Record row')
sequenceRecord a = Builder.build <@> {} <$> builder
  where
    builder = sequenceRecordImpl (Proxy :: Proxy rl) a
    
