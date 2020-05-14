module ExtractKeys where

import Prelude

import Data.List (List, (:), toUnfoldable)
import Prim.RowList (kind RowList, Cons, Nil)
import Type.Prelude (RLProxy(..), reflectSymbol, class IsSymbol, SProxy(..), class RowToList)

class Keys (xs :: RowList) where
  keysImpl :: RLProxy xs -> List String

instance nilKeys :: Keys Nil where
  keysImpl _ = mempty

instance consKeys :: (IsSymbol name, Keys tail) => Keys (Cons name ty tail) where
  keysImpl _ = first : rest
    where
      first = reflectSymbol(SProxy :: SProxy name)
      rest = keysImpl (RLProxy :: RLProxy tail)
      
extractKeys :: forall row rl. RowToList row rl => Keys rl => Record row -> Array String
extractKeys _ = toUnfoldable $ keysImpl (RLProxy :: RLProxy rl)

