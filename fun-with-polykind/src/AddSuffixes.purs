module AddSuffixes (addSuffixes, class Suffixable, addSuffixes') where

import Data.Symbol (class IsSymbol)
import Prim.Symbol (class Append)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Nil, Cons) as RL
import Record (insert, get, delete)
import Type.Proxy (Proxy(..))

__ :: forall sym. Proxy sym
__ = Proxy

class Suffixable :: forall k1 k2. k1 -> k2 -> Row Type -> Row Type -> Constraint
class Suffixable ns suf row alt | -> alt where
  addSuffixes' :: Proxy ns -> Proxy suf -> Record row -> Record alt
  
instance sufNil :: Suffixable RL.Nil suf row row where
  addSuffixes' _ _ rec = rec

-- | rec :: { cur :: typ, next1 :: typ1, next2 :: typ2 ...}
-- | row :: ( cur :: typ, next1 :: typ1, next2 :: typ2 ...)
-- |
-- | nxt :: { next1 :: typ1, next2 :: typ2 ...}
-- | now :: ( next1 :: typ1, next2 :: typ2 ...)
-- | ns ::  [ next1 :: typ1, next2 :: typ2 ...]
-- |
-- | itr :: { next1suf :: typ1, next2suf :: typ2 ...}
-- | iow :: ( next1suf :: typ1, next2suf :: typ2 ...)
-- |
-- | alt :: (cursuf :: typ, next1suf :: typ1, next2suf :: typ2 ...)

instance sufCons :: 
  ( Append cur suf cursuf
  , IsSymbol cur
  , IsSymbol cursuf
  , Cons cursuf typ iow alt 
  , Lacks cursuf iow
  , Suffixable ns suf now iow
  , Cons cur typ now row
  , Lacks cur now
  ) 
  => Suffixable (RL.Cons cur typ ns) suf row alt where
  addSuffixes' _ psuf rec =
    let nxt = delete (__ :: _ cur) rec :: Record now
        itr = addSuffixes' (Proxy :: Proxy ns) psuf nxt :: Record iow
      in insert (__ :: _ cursuf) (get (__ :: _ cur) rec) itr :: Record alt  

addSuffixes 
  :: forall xs suf row alt
   . Suffixable xs suf row alt
  => RL.RowToList row xs
  => Proxy suf -> Record row -> Record alt
addSuffixes psuf rec =
  addSuffixes' (Proxy :: Proxy xs) psuf rec



