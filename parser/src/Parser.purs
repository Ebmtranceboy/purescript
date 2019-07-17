module Parser(module Parser) where

import Prelude

import Data.String(Pattern(..), drop, take, stripPrefix, length, toUpper, toLower)
import Control.Monad.State.Trans(StateT, runStateT)
import Control.Monad.Writer.Trans(WriterT, runWriterT)
import Data.Identity(Identity)
import Control.Monad.Except.Trans(ExceptT, runExceptT, throwError)
import Control.Monad.State(get, put)
import Control.Monad.Writer(tell)
import Control.Monad.Trans.Class(lift)
import Data.Newtype(unwrap)
import Data.Either(Either)
import Data.Tuple(Tuple)
import Data.Maybe(Maybe(..))
import Control.MonadPlus(guard)
import Data.Array(some)
import Control.Alternative((<|>))

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runParser :: forall a. Parser a -> String -> Either Errors (Tuple (Tuple a String) Log)
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)


string :: String -> Parser String
string str = do
  s <- get
  lift $ tell ["The state is " <> s]
  case stripPrefix (Pattern str) s of
    Nothing -> lift $ lift $ throwError ["Unmatching pattern"]
    _ -> do
      let n = length str
      put (drop n s)
      pure (take n s)

upper :: Parser String
upper = do
  s <- split
  guard $ toUpper s == s
  pure s

lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  pure s

upperOrLower :: Parser (Array String)
upperOrLower = some upper <|> some lower
