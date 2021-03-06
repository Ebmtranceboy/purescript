module RecordMap (recordMap, class Mappable, recordMap') where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons) as RL
import Record (insert, get, delete)
import Type.Proxy (Proxy(..))

__ :: forall sym. Proxy sym
__ = Proxy

class Mappable :: forall k. (Type -> Type) -> k -> Row Type -> Row Type -> Constraint
class Mappable f xs rx ry | -> ry where
  recordMap' ::  (forall a. a -> f a) -> Proxy xs -> Record rx -> Record ry
 
instance mapNil 
  :: Mappable f RL.Nil ry ry where
  recordMap' _ _ rec = rec
  
instance mapCons
  :: ( IsSymbol k
     , Mappable f xst rxt ryt
     , Cons k xtyp rxt rx
     , Lacks k rxt
     , Cons k (f xtyp) ryt ry
     , Lacks k ryt
     ) => Mappable f (RL.Cons k xtyp xst) rx ry where
  recordMap' f xs recx =
    let next = delete (__ :: _ k) recx :: Record rxt
        itr = recordMap' f (Proxy :: Proxy xst) next :: Record ryt
    in insert (__ :: _ k) (f $ get (__ :: _ k) recx) itr :: Record ry

recordMap
  :: forall xs rx ry f
   . Mappable f xs rx ry 
   => RL.RowToList rx xs
   => (forall a. a -> f a) -> Record rx -> Record ry
recordMap f recx = recordMap' f (Proxy :: Proxy xs) recx

