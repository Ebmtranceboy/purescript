module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

newtype Fix f = Fix (f (Fix f))

unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix x) = x

type Algebra f a = f a -> a

data Exp a = Const Number
  | Var
  | Plus a a
  | Times a a

derive instance functorExp :: Functor Exp

cata :: forall f a. Functor f => Algebra f a -> Fix f -> a
cata algbr t = algbr $ map (cata algbr) $ unFix t

alg :: Number -> Exp Number -> Number
alg _ (Const c) = c
alg val Var = val
alg _ (Plus a b) = a + b
alg _ (Times a b) = a * b

num :: Number -> Fix Exp
num x = Fix (Const x)

var :: Fix Exp
var = Fix Var

plus :: Fix Exp -> Fix Exp -> Fix Exp
plus a b = Fix (Plus a b)

times :: Fix Exp -> Fix Exp -> Fix Exp
times a b = Fix (Times a b)

exp :: Fix Exp
exp = times (plus (num 4.0) var) (plus (num 1.0) var)

main :: Effect Unit
main = do
  log $ show $ cata (alg 3.0) exp
