module Rec (class RecConToFun, recConToFun_, rec, Example (Example)) where

import Prelude

import Data.Symbol       (SProxy(..), class IsSymbol)
import Prim.Row          (class Lacks, class Cons)
import Prim.RowList      (class RowToList, Cons, Nil)
import Record            (insert)
import Type.Data.Row     (RProxy(..))
import Type.Data.RowList (RLProxy(..))

class RecConToFun rest list return fun final
  | rest list return -> fun
  , rest list -> final
  , fun -> return
 where
  recConToFun_ :: RLProxy list
               -> Record rest
               -> (Record final -> return)
               -> fun

instance rtfNil :: RecConToFun rest Nil a a rest where
  recConToFun_ _ = (#)

instance rtfCons :: ( Cons label value rest rest'
                    , Lacks label rest
                    , RecConToFun rest' list a fun final
                    , IsSymbol label
                    )
                 => RecConToFun rest (Cons label value list) a (value -> fun) final
 where
  recConToFun_ _ rest con value = recConToFun_
    (RLProxy :: RLProxy list)
    (insert (SProxy :: SProxy label) value rest)
    con

rec :: forall row list return fun final
     . RowToList row list
    => RecConToFun () list return fun row
    => (Record row -> return)
    -> fun
rec = recConToFun_ (RLProxy :: RLProxy list) {}

data Example = Example { foo :: Int, bar :: String }

instance showExample :: Show Example where
  show (Example row) = "Example " <> show row