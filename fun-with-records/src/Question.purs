module Question where

import Prim.RowList as RL
import Prim.Row as R
import Prim.Symbol as S
import Data.Symbol as S
import Data.Symbol (SProxy(..))
import Type.Data.RowList (RLProxy(..))

import Record

import Prelude (($))


class Flatten (rl :: RL.RowList) (r1 :: # Type) (r2_a :: # Type) (r2_b :: # Type) (prefix :: Symbol)
    | rl r1 r2_a prefix -> r2_b where
    flattenImpl :: SProxy prefix -> RLProxy rl -> Record r1 -> Record r2_a -> Record r2_b

instance flattenNil :: Flatten RL.Nil r1 r r p where
    flattenImpl _ _ _ r = r

else instance flattenConsRecord :: 
    ( RL.RowToList t t_rl
    , S.Append p l p'
    , S.Append p' "." p''
    , Flatten t_rl t r2_a r2_b' p''

    , Flatten tail r1 r2_b' r2_b p

    , S.IsSymbol l
    , R.Cons l (Record t) trash r1
    ) => Flatten (RL.Cons l (Record t) tail) r1 r2_a r2_b p where
    flattenImpl _ _ r1 r2_a = flattenImpl (SProxy :: _ p  ) (RLProxy :: _ tail) r1 $
                              flattenImpl (SProxy :: _ p'') (RLProxy :: _ t_rl) (get (SProxy :: _ l) r1) r2_a

else instance flattenConsElse ::
    ( S.Append p l l'
    , S.IsSymbol l'
    , R.Lacks l' r2_a
    , R.Cons l' t r2_a r2_b'

    , S.IsSymbol l
    , R.Cons l t trash r1

    , Flatten tail r1 r2_b' r2_b p

    ) => Flatten (RL.Cons l t tail) r1 r2_a r2_b p where
    flattenImpl _ _ r1 r2_a = flattenImpl (SProxy :: _ p  ) (RLProxy :: _ tail) r1 r2_b'
        where
            r2_b' = insert (SProxy :: _ l') (get (SProxy :: _ l) r1) r2_a


flatten :: forall r1 r2 rl r2_b
    . RL.RowToList r1 rl
   => Flatten rl r1 () r2_b ""
   => Record r1 -> Record r2_b
flatten r1 = flattenImpl (SProxy :: _ "") (RLProxy :: _ rl) r1 {}
