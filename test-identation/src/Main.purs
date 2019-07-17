module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Array(range)
import Data.Foldable(foldr)
import Data.Traversable(sequence)
import Control.Monad.Reader(Reader, runReader, ask, local)

type Level = Int
type Doc = Reader Level String

spaces :: Int -> String
spaces 0 = ""
spaces n = " " <> spaces (n-1)

line :: String -> Doc
line str = do
  level <- ask
  let blanks = spaces $ level * 2
  pure $ blanks <> str

indent :: Doc -> Doc  
indent = local ( _ + 1 )

cat :: Array Doc -> Doc
cat arr = map (foldr (\ a b -> a <> "\n" <> b) "") $ sequence arr

render :: Doc -> String
render doc = runReader doc 0


test = render $ cat
  [ line "Here is some indented text:"
  , indent $ cat
      [ line "I am indented"
      , line "So am I"
      , indent $ line "I am even more indented"
      ]
  ]

main :: Effect Unit
main = do
  log test
