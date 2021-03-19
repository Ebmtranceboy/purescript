module RcatMaybes where

--import Prelude
import Prelude ((<>))



--import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record as R
import Type.Proxy (Proxy(..))

class Predicatable a where
  predicate :: a -> Boolean

class Labels :: forall k. Row Type -> k -> Constraint
class Labels row list | list -> row where
  labelsImpl :: Proxy list -> { | row } -> Array String
  
instance labelsNil :: Labels () RL.Nil where
  labelsImpl _ _ = []

instance labelsCons :: 
  ( RL.RowToList row list
  , IsSymbol l
  , Lacks l row'
  , Cons l a row' row
  , RL.RowToList row' list'
  , Labels row' list'
  , Predicatable a
  ) => Labels row (RL.Cons l a list') where
  labelsImpl _ record = arr 
   where
    keyS = Proxy ∷ Proxy l
    key = reflectSymbol keyS
    value = R.get keyS record
    record' :: { | row' }
    record' = R.delete keyS record
    arr' = labelsImpl (Proxy ∷ Proxy list') record'
    arr = 
      if predicate value
        then [key] <> arr'
        else arr'
  
labels :: forall row list
   .  RL.RowToList row list
   => Labels row list
   => { | row } -> Array String
labels =labelsImpl (Proxy :: Proxy list)

{-----------------------}

--ra = R.rename (Proxy :: Proxy "a")
_a :: Proxy "a"
_a = Proxy 

truc = R.insert _a _a {}
-- $ ra (Proxy :: Proxy "b") {a: 7, b: true}

-- types à l'existence conceptuelle uniquement
type Non_concrétisable_a r = ( aire :: Number | r )
type Non_concrétisable_b = ( bordure :: Boolean )
type Non_concrétisable_bc r = ( bordure :: Boolean, côté :: Number | r )
type Non_concrétisable_abc r = Non_concrétisable_a (Non_concrétisable_bc r)

-- type concret ouvert
type Enregistrement r = Record (Non_concrétisable_abc r)

-- type concret fermé
cube :: Enregistrement (volume :: Number)
cube = { côté: 2.0, aire: 24.0, volume: 8.0, bordure: false }

-- type concret fermé
carré :: Enregistrement ()
carré = { côté: 2.0, aire: 4.0, bordure: true }


class RecordApplyWithLabelss :: forall k1 k2. k1 -> Row Type -> k2 -> Row Type -> Row Type -> Constraint
class RecordApplyWithLabelss fs rf xs rx ry | -> ry where
  recordApplyImpl :: Array String -> Proxy fs -> { | rf }
                                  -> Proxy xs -> { | rx } -> {-Either { | rx }-} { | ry }

instance applyNil
  :: RecordApplyWithLabelss RL.Nil rf RL.Nil ry ry where
  recordApplyImpl _ _ _ _ rec = {-Right-} rec

instance applyCons ::
  ( IsSymbol k
  , RecordApplyWithLabelss fst rft xst rxt ryt
  , Cons k xtyp rxt rx
  , Lacks k rxt
  , Cons k (xtyp -> ytyp) rft rf
  , Lacks k rft
  , Cons k ytyp ryt ry
  , Lacks k ryt
  ) => RecordApplyWithLabelss 
                 (RL.Cons k (xtyp -> ytyp) fst) rf
                 (RL.Cons k xtyp xst) rx ry where
  recordApplyImpl keys fs recf xs recx =
    let key = Proxy :: Proxy k
        nextf = R.delete key recf :: { | rft }
        nextx = R.delete key recx :: { | rxt }
        itr = recordApplyImpl keys (Proxy :: Proxy fst) nextf
                           (Proxy :: Proxy xst) nextx :: { | ryt }
    in {-if (reflectSymbol key) `Array.elem` keys
                  then Right $-} R.insert 
                        key 
                        (R.get key recf (R.get key recx))
                        itr
                  -- else {-Left-} itr
    
recordApplyWithLabelss
  :: forall fs rf xs rx ry
   . RecordApplyWithLabelss fs rf xs rx ry
   => RL.RowToList rf fs
   => RL.RowToList rx xs
   => Array String -> { | rf } -> { | rx } -> {-Either { | rx }-} { | ry }
recordApplyWithLabelss keys recf recx =
  recordApplyImpl keys
                  (Proxy :: Proxy fs) recf
                  (Proxy :: Proxy xs) recx


