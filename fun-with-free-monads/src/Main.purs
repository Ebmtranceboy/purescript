module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

data My a = Zero | One a | Two a a
                 
instance myShow :: Show a => Show (My a) where
  show Zero = "Zero"
  show (One x) = "One " <> show x
  show (Two x y) = "Two " <> show x <> " " <> show y

instance myFunctor :: Functor My where
  map f (Zero) = Zero
  map f (One x) = Two (f x) (f x)
  map f (Two x y) = One (f y)

data Free f a = Pure a | Roll (f (Free f a))

instance freeShow :: Show a  => Show (Free f a) where
  show (Pure a) = "Pure " <> show a
  show (Roll x) = "Roll"


instance freeFunctor :: Functor f => Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f (Roll x) = Roll (map (map f) x)

concatFree :: forall f a. Functor f => Free f (Free f a) -> Free f a
concatFree (Pure x) = x
concatFree (Roll y) = Roll (map concatFree y)

bindFree :: forall a b f. Functor f => Free f a -> (a -> Free f b) -> Free f b
bindFree x f = concatFree (map f x)

instance freeApply :: ( Functor f)  => Apply (Free f) where
  apply ff fx =  bindFree fx (\x -> bindFree ff (\f -> Pure $ f x)) 
   
instance freeApplicative :: Functor f => Applicative (Free f) where
  pure = Pure

instance freeBind :: Functor f => Bind (Free f) where
  bind x f = bindFree x f 

type MyMonad a = Free My a

liftFree :: forall a f. Functor f => f a -> Free f a
liftFree x = Roll (map Pure x)

foldFree :: forall f r. Functor f => (f r -> r) -> Free f r -> r
foldFree _ (Pure a) = a
foldFree f (Roll x) = f (map (foldFree f) x)

extract :: forall a. Monoid a => My a -> a
extract Zero = mempty
extract (One x) = x
extract (Two x y) = x <> y

test0 :: MyMonad String
test0 = do
  n <- (\x -> x <> x) <$> (liftFree $ One "A")
  pure n

test1 :: MyMonad (Array Int)
test1 = do
  n <- (\x -> x <> x) <$> (liftFree $ Two [12] [5])
  pure n


main :: Effect Unit
main = do
  logShow $ One 2
  logShow $ (_+4) <$> One 2
  logShow $ foldFree extract test0
  logShow $ foldFree extract test1


