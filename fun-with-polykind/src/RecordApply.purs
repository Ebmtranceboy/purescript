module RecordApply (recordApply, class Applyable, recordApply') where

import Prelude

import Record (insert, get, delete)
import Data.Int (toNumber)
import Data.String (length)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons) as RL
import Type.Proxy (Proxy(..))

__ :: forall sym. Proxy sym
__ = Proxy

class Applyable :: forall k1 k2. k1 -> Row Type -> k2 -> Row Type -> Row Type -> Constraint
class Applyable fs rf xs rx ry | -> ry where
  recordApply' :: Proxy fs -> Record rf -> Proxy xs -> Record rx -> Record ry
 
instance applyNil 
  :: Applyable RL.Nil rf RL.Nil ry ry where
  recordApply' _ _ _ rec = rec
  
instance applyCons ::
  ( IsSymbol k
  , Applyable fst rft xst rxt ryt
  , Cons k xtyp rxt rx
  , Lacks k rxt
  , Cons k (xtyp -> ytyp) rft rf
  , Lacks k rft
  , Cons k ytyp ryt ry
  , Lacks k ryt
  ) => Applyable (RL.Cons k (xtyp -> ytyp) fst) rf 
                 (RL.Cons k xtyp xst) rx ry where
  recordApply' fs recf xs recx =
    let nextf = delete (__ :: _ k) recf :: Record rft
        nextx = delete (__ :: _ k) recx :: Record rxt
        itr = recordApply' (__ :: _ fst) nextf 
                           (__ :: _ xst) nextx :: Record ryt
    in insert (__ :: _ k) 
              (get (__ :: _ k) recf $ get (__ :: _ k) recx) itr :: Record ry

recordApply
  :: forall fs rf xs rx ry 
   . Applyable fs rf xs rx ry 
   => RL.RowToList rf fs
   => RL.RowToList rx xs
   => Record rf -> Record rx -> Record ry
recordApply recf recx = recordApply' (__ :: _ fs) recf 
                                     (__ :: _ xs) recx


