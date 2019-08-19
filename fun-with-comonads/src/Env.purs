module Env where

import Prelude
import Control.Comonad(class Comonad, class Extend, duplicate, extract)

data Env e a = Env e a

instance eqEnv :: (Eq a, Eq e) => Eq (Env e a) where
  eq (Env e a) (Env e' a') = e==e' && a==a'

instance showEnv :: (Show e, Show a) => Show(Env e a) where
  show (Env e a) = "Env " <> show e <> " " <> show a

instance functorEnv :: Functor (Env e) where
  map f (Env e a) = Env e (f a)
  -- that is : map f = extend (f <<< duplicate)

extract' :: forall e a. Env e a -> a
extract' (Env _ a) = a

duplicate' :: forall e a. Env e a -> Env e (Env e a) 
duplicate' (Env e a) = Env e (Env e a)
-- that is : duplicate = extend identity

instance extendEnv :: Extend (Env e) where
  extend :: forall e a b. (Env e a -> b) -> Env e a -> Env e b
  -- that is : --extend f = map f <<< duplicate
  extend f (Env e a) = Env e (f (Env e a))

instance comonadEnv :: Comonad (Env e) where
  extract = extract'

class Comonad (env e) <= Environment env e a where
  ask :: env e a -> e
  asks :: forall e'. (e -> e') -> env e a -> e'
  local :: forall e'. (e -> e') -> env e a -> env e' a

instance envEnv :: Environment Env e a where
  ask (Env e _) = e
  asks f (Env e _) = f e
  local f (Env e a) = Env (f e) a


