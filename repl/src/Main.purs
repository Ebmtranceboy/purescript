module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Record(nub,insert,union) as Record
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Cons, class Lacks, class Nub, class Union)

{-
class ( IsSymbol l
      , Lacks l ()
      , Cons l a () r__
      , Union r__ rI r___
      , Nub r___ rO
      ) <= Upsert l a rI rO r__ r___

upsert
  :: forall l a rI rO r__ r___
     . Upsert l a rI rO r__ r___
     => SProxy l
  -> a
  -> { |rI}
  -> { |rO}
 -} 
--upsert p v obj = Record.merge (Record.insert p v ({} :: Record ())) obj

upsert :: forall l a rI rO r__ r___. IsSymbol l => Lacks l () => Cons l a () r__ => Union r__ rI r___ => Nub r___ rO => SProxy l -> a -> Record rI -> Record rO
upsert p v obj = Record.nub $ Record.union (Record.insert p v {}) obj

test1 :: {a :: Int}
test1 = upsert (SProxy :: SProxy "a") 7 {a: "s"}

main :: Effect Unit
main = do
  --log $ show test1
  --log $ show $ upsert (SProxy :: SProxy "a") 7 {}
  pure unit
  

