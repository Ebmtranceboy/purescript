module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
--import Data.Array(head)
import Data.String(Pattern(..), drop, take, stripPrefix, length)
import Data.Maybe(Maybe(..))
import Data.Either(Either)
import Data.Tuple(Tuple(..))
import Data.Foldable(foldr)
import Data.Traversable(sequence)
import Control.Monad.State(get, put)
import Control.Monad.Reader(Reader, ask, local, runReader)
import Control.Monad.Writer(tell)
import Data.Identity(Identity)
import Data.Newtype(unwrap)
import Control.Monad.State.Trans(StateT, runStateT)
import Control.Monad.Reader.Trans(ReaderT, runReaderT)
import Control.Monad.Writer.Trans(WriterT, runWriterT)
import Control.Monad.Except.Trans(ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class(lift)

{- Transformation 0 -}

safeDivideTrans :: Number -> Number -> ExceptT String Identity Number
safeDivideTrans num den = if den == 0.0 then throwError "Division by Zero" else pure (num / den)

safeDivide :: Number -> Number -> Either String Number
safeDivide num den = unwrap $ runExceptT $ safeDivideTrans num den

{- Transformation 1 -}

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runParser :: (Parser String) -> String -> Either Errors (Tuple (Tuple String String) Log)
runParser p s = unwrap $ runExceptT $ runWriterT $ runStateT p s

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

{- Transformation 2 -}

type Level = Int
type Doc = Reader Level String

blankPrefix :: Int -> String -> String
blankPrefix 0 str = str
blankPrefix n str = " " <> blankPrefix (n-1) str

withNewLine :: String -> String -> String
withNewLine a b = a <> "\n" <> b

line :: String -> Doc
line str = do
  level <- ask
  pure $ blankPrefix (2*level) str

indentDoc :: Doc -> Doc
indentDoc = local (_ + 1) 

cat :: Array Doc -> Doc
cat arr = (foldr withNewLine "") <$> (sequence arr)

render :: Doc -> String
render doc = runReader doc 0

{- Transformation 3 -}

type Builder = ReaderT Level (WriterT (Array String) Identity) Unit

runBuilder :: Level -> Builder -> Tuple Unit (Array String)
runBuilder l b = unwrap $ runWriterT $ runReaderT b l 

indentBuilder :: Builder -> Builder
indentBuilder = local (_ + 1) 

build :: String -> Builder
build str = do
  level <- ask
  {-lift $-}
  tell [blankPrefix (2*level) str <> "\n"]
  
main :: Effect Unit
main = do
  log "\nTest 0:"
  logShow $ safeDivide 1.0 0.0
  logShow $ safeDivide 1.0 2.0

  log "\nTest 1:"
  logShow $ runParser (string "abc") "abcdef"

  log "\nTest 2:"
  log $ render $ cat
    [ line "Here is some indented text:"
    , indentDoc $ cat
      [ line "I am indented"
      , line "So am I"
      , indentDoc $ line "I am even more indented"
      ]
    ]

  log "\nTest 3:"
  log $ (\(Tuple un arr) -> foldr (<>) "" arr) $ runBuilder 2 $ do
    build "Here is some indented text:"
    indentBuilder $ do
      build "I am indented"
      build "So am I"
      indentBuilder $ build "I am even more indented"


