-- install typelevel-eval
import Type.Eval  (class Eval, kind TypeExpr, Lift1) as T
import Type.Eval.RowList (FromRow, ToRow) as T
import Type.Eval.Functor (Map) as T
import Type.Eval.ValueOf (ValueOf) as T
import Type.Row (RProxy)

foreign import data ToRecord :: Type -> T.TypeExpr
instance evalToRecord :: T.Eval (ToRecord (RProxy r)) (Record r)
type Trans t = ToRecord T.<<< T.ToRow T.<<< T.Map (T.Lift1 t) T.<<< T.FromRow
t1 :: T.ValueOf (Trans Maybe (RProxy (a :: Int, b :: Int)))
t1 = T.from { a: Just 10, b: Nothing }
t2 :: { a :: Maybe Int, b :: Maybe Int}
t2 = T.to t1

-- simpler :
type Unlifted a = a
type MyRows (f :: Type -> Type) =
  ( a :: f Int
  , b :: f Int
  )
type A = { | MyRows Unlifted }
type B = { | MyRows Maybe }