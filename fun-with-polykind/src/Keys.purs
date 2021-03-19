module Keys (keys, class Keys, keysImpl) where

import Prelude

import Data.List (List, (:))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class Keys (xs :: RL.RowList Type) where
  keysImpl :: Proxy xs -> List String

instance nilKeys :: Keys RL.Nil where
  keysImpl _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (RL.Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol (Proxy :: Proxy name)
      rest = keysImpl (Proxy :: Proxy tail)

keys :: forall g row rl
   . RL.RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> List String
keys _ = keysImpl (Proxy :: Proxy rl)

