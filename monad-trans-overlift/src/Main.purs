module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)

--import Control.Monad.Trans.Class(lift)
import Data.Identity(Identity)
import Control.Monad.Except.Trans(ExceptT, runExceptT, throwError)
import Data.Newtype(unwrap)
import Data.Either(Either)
--import Control.Monad.Writer
--import Control.Monad.Writer.Class
--import Control.Monad.Error.Class

safeDivideTrans :: Number -> Number -> ExceptT String Identity Number
safeDivideTrans num den = if den == 0.0 then throwError "Division by Zero" else pure (num / den)

safeDivide :: Number -> Number -> Either String Number
safeDivide num den = unwrap $ runExceptT $ safeDivideTrans num den

type Errors = Array String
type Log = Array String
--type Parser = StateT String (WriterT Log (ExceptT Errors Identity))


main :: Effect Unit
main = do
  logShow $ safeDivide 1.0 0.0
  logShow $ safeDivide 1.0 2.0
