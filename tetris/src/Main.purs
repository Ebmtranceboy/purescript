module Main where

import Prelude
import Data.Int (toNumber)
import Data.Array ((!!),(\\),(..),any,length,filter,head,all)
import Data.Maybe (fromJust)
import Data.Foldable(or,foldr,foldl)
import Control.Alt (alt)
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Random(randomInt)
import FRP.Behavior (Behavior, animate, unfold)
import FRP.Event.Keyboard(down)
import FRP.Event.Time(interval)
import Graphics.Canvas (getCanvasElementById, getContext2D
                       , getCanvasHeight, getCanvasWidth
                       , setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (Drawing, render
                        , rectangle
                        , fillColor, filled)
import Color (rgb)

aVeryBigPowerOf2 = 33554432 :: Int -- 2^25

class Random a where
  random :: {val::a, gen::a} -> {val::a, gen::Int} -> {val::a, gen::Int}
  dummy :: a

instance randomInt :: Random Int where
  dummy = 0
  random {val: t, gen: h} {val: _, gen: x} = 
    {val: t + y `mod` (h-t+1), gen: y}
       where m = aVeryBigPowerOf2
             y = (x * (4*x+1) + 1) `mod` m

type Colour = {r:: Int, g:: Int, b:: Int}
type Pos = {x:: Int, y::Int}

data Piece = Piece { offsets :: Array Pos
                   , pos :: Pos
                   , col :: Colour}

piece :: Array Pos -> Pos -> Colour -> Piece
piece off pos col = Piece {offsets: off, pos: pos, col: col}
 
startPos :: Pos
startPos = {x: 4, y:19}

constructs :: Array (Pos -> Colour -> Piece)
constructs = [ piece [{x:0,y:0},{x:0,y: -1},{x:0,y: -2},{x:0,y: -3}]
             , piece [{x:0,y:0},{x:1,y:0},{x:0,y: -1},{x:0,y: -2}]
             , piece [{x:0,y:0},{x: -1,y:0},{x:0,y: -1},{x:0,y: -2}]
             , piece [{x:0,y:0},{x:1,y:0},{x: -1,y:0},{x:0,y:1}]
             , piece [{x:0,y:0},{x:1,y:0},{x:1,y: -1},{x:0,y: -1}]
             , piece [{x:0,y:0},{x:0,y: -1},{x:1,y: -1},{x: -1,y:0}]
             , piece [{x:0,y:0},{x:0,y: -1},{x: -1,y: -1},{x:1,y:0}]]
  
instance randomPiece :: Random Piece where
   dummy = (unsafePartial $ fromJust $ constructs !! 0) startPos {r: 255, g: 0, b: 0}
   random _ {val: _, gen: g} = 
      let {val: i, gen: g'} = random {val: 0, gen: length constructs-1} 
                                     {val: dummy, gen: g}
          {val: r, gen: g''} = random {val: 0, gen: 200} 
                                      {val: dummy, gen: g'}
          {val: gr, gen: g'''} = random {val: 0, gen: 200} 
                                        {val: dummy, gen: g''}
          {val: b, gen: g''''} = random {val: 0, gen: 200} 
                                        {val: dummy, gen: g'''}
      in { val: (unsafePartial $ fromJust $ constructs !! i) 
                   startPos 
                   {r, g: gr, b}
         , gen: g''''} 
 
type Board = Array {pos:: Pos, col:: Colour}
type Game = {piece:: Piece, board:: Board}

side :: Number
side = 20.0

gameRows :: Int
gameRows = 20
gameColumns :: Int
gameColumns = 10

vertbounds :: Int
vertbounds = gameRows-1
horizbounds :: Int
horizbounds = gameColumns-1

whiteSquares :: Board
whiteSquares = do
  x <- 0..horizbounds
  y <- 0..vertbounds
  pure {pos: {x, y}, col:{r:255, g: 255, b: 255}}

pieceToSquares :: Piece -> Board
pieceToSquares (Piece p) = 
     map (\{x,y} -> {pos:{x:x+x',y:y+y'},col: c}) p.offsets 
                 where {x: x', y: y'} = p.pos
                       c = p.col

type State = {currentSeed :: Int
             , nextPiece :: Piece
             , currentGame :: Game}

renderState :: State -> Drawing
renderState {currentSeed:_, nextPiece, currentGame: {piece: p, board}} = 
  let squareToBox :: Int -> Int -> {pos:: Pos, col:: Colour} -> Drawing
      squareToBox offx offy {pos: {x,y}, col: c} = 
          filled (fillColor $ rgb c.r c.g c.b) 
                 (rectangle ((toNumber $ x+offx)*side) 
                            ((toNumber $ offy-y)*side) 
                            side 
                            side)

  -- blank
  in foldr (<>) mempty $ (squareToBox 5 23 <$> whiteSquares)
  -- board
  <> (squareToBox 5 23 <$> board)
  -- piece
  <> (squareToBox 5 23 <$> (pieceToSquares p))
  -- preview
  <> (squareToBox (-2) 29 <$> (pieceToSquares nextPiece))

renderPreview :: Piece -> Drawing
renderPreview p = 
  let squareToBox :: {pos:: Pos, col:: Colour} -> Drawing
      squareToBox {pos: {x,y}, col: c } = 
          filled (fillColor $ rgb c.r c.g c.b) 
                 (rectangle ((toNumber $ x-2)*side) 
                            ((toNumber $ 19-y+10)*side) 
                            side 
                            side)
  in foldr (<>) mempty $ (squareToBox <$> pieceToSquares p)

floorCollision :: Piece -> Boolean
floorCollision p = 
   let rs = (_.pos) <$> (pieceToSquares p)
   in or $ do
      {x:_,y} <- rs 
      pure $ y < 0

ceilingCollision :: Piece -> Boolean
ceilingCollision p = 
  let rs = (_.pos) <$> (pieceToSquares p)
  in or $ do
    {x:_,y} <- rs
    pure $ y > vertbounds

boardCollision :: Piece -> Board -> Boolean
boardCollision p b = 
  let rs = (_.pos) <$> (pieceToSquares p) 
      rs' = (_.pos) <$> b
  in or $ do
    r <- rs
    r' <- rs'
    pure $ r==r'

wallCollision :: Piece -> Boolean
wallCollision p = 
  let rs = (_.pos) <$> (pieceToSquares p)
  in or $ do
    {x,y:_} <- rs
    pure $ x<0 || x > horizbounds

guardTransform :: Board -> (Piece -> Piece) -> Piece -> Piece
guardTransform b f p = if wallCollision (f p) || floorCollision (f p) 
                         || boardCollision (f p) b 
                       then p else f p

emptyBoard :: Board
emptyBoard = []

unusedRows :: Board -> Array Int
unusedRows b = filter (\j -> 
    not $ any (\{pos: {x:_,y:j'},col:_} -> j==j') b
    ) $ 0..vertbounds

lowestUnused :: Board -> Int 
lowestUnused b = unsafePartial (fromJust $ head $ unusedRows b)

changeRow :: Int -> Int -> Board -> Board
changeRow j j' = 
    map (\{pos:{x:i,y:j''},col:v} -> 
        if j''==j 
           then {pos:{x:i,y:j'},col:v} 
           else {pos:{x:i,y:j''},col:v})

dropRow :: Board -> Int -> Board
dropRow b j = 
     if lowestUnused b < j 
     then changeRow j (lowestUnused b) b 
     else b

dropRows :: Board -> Board
dropRows b = foldl dropRow b $ 0..vertbounds 

clearRow :: Board -> Int -> Board
clearRow b j = b \\ filter (\{pos:{x:_,y:j'},col:_} -> j'==j) b

lookup :: Pos -> Board -> Boolean
lookup {x,y} = any (\{pos:{x:x', y:y'},col:_} -> x == x' && y == y')

fullRow :: Board -> Int -> Boolean
fullRow b j = all (\i -> lookup {x:i,y:j} b) $ 0..horizbounds 

fullRows :: Board -> Array Int
fullRows b = filter (fullRow b) $ 0..vertbounds

boardHandler :: Board -> Board
boardHandler b = dropRows $ foldl clearRow b $ fullRows b

addToBoard :: Piece -> Board -> Board
addToBoard p b = pieceToSquares p <> b

{-
--constructs = [doubleU5,rightAsymZ5,rightSymZ5,plus5,leftLt5,p5,leftSymZ5,lefttL5
--             ,t5,corner5,leftAsymZ5,c5,q5,rightL5,leftL5,rightt5,leftt5,straight5]
doubleU5,rightAsymZ5,rightSymZ5,plus5,leftLt5,p5,leftSymZ5,lefttL5 
  ,t5,corner5,leftAsymZ5,c5,q5,rightL5,leftL5,rightt5,leftt5,straight5 :: Shape
doubleU5 = Piece [(0,1),(0,2),(1,1),(1,0),(2,0)]
rightAsymZ5 = Piece [(1,1),(1,2),(1,3),(0,1),(0,0)]
rightSymZ5 = Piece [(1,0),(1,1),(1,2),(2,0),(0,2)]
plus5  = Piece [(1,0),(1,1),(1,2),(2,1),(0,1)]
leftLt5= Piece [(1,0),(1,1),(1,2),(2,1),(0,0)]
p5 = Piece [(0,0),(0,1),(0,2),(1,1),(1,2)]
leftSymZ5 = Piece [(1,0),(1,1),(1,2),(2,2),(0,0)]
lefttL5 = Piece [(1,0),(1,1),(1,2),(2,0),(0,1)]
t5 = Piece [(1,0),(1,1),(1,2),(2,0),(0,0)]
corner5 = Piece [(0,0),(0,1),(0,2),(1,0),(2,0)]
leftAsymZ5 = Piece [(0,1),(0,2),(0,3),(1,1),(1,0)]
c5 = Piece [(0,0),(0,1),(0,2),(1,0),(1,2)]
q5 = Piece [(0,0),(0,1),(0,2),(1,0),(1,1)]
rightL5 = Piece [(0,1),(0,2),(0,3),(0,0),(1,0)]
leftL5 = Piece [(0,1),(0,2),(0,3),(0,0),(1,3)]
rightt5 = Piece [(0,1),(0,2),(0,3),(0,0),(1,2)]
leftt5 = Piece [(0,1),(0,2),(0,3),(0,0),(1,1)]
straight5 = Piece [(0,2),(0,3),(0,4),(0,0),(0,1)]
-}

moveLeft :: Piece -> Piece
moveLeft (Piece p) = let {x,y} = p.pos in Piece p{pos={x:x-1,y}}
moveRight :: Piece -> Piece
moveRight (Piece p) = let {x,y} = p.pos in Piece p{pos={x:x+1,y}}
moveDown :: Piece -> Piece
moveDown (Piece p) = let {x,y} = p.pos in Piece p{pos={x,y:y-1}}

counterClock' :: Pos -> Pos
counterClock' {x,y} = {x: -y,y:x}

counterClock :: Piece -> Piece
counterClock (Piece p) = Piece p{offsets=os'}
    where os = p.offsets
          os' = map counterClock' os

movePiece :: String -> Board -> Piece -> Piece
movePiece "ArrowRight" b = guardTransform b moveRight
movePiece "ArrowLeft" b = guardTransform b moveLeft
movePiece "ArrowDown" b = guardTransform b moveDown
movePiece "ArrowUp" b = guardTransform b counterClock
movePiece _ _ = identity

refreshGame :: String -> Game -> Game
refreshGame str g = 
    let p = movePiece str g.board g.piece 
    in {piece: p, board: g.board}

initialState :: Int -> State
initialState seed = 
   let {val, gen} = random {val: dummy, gen: dummy} {val: dummy, gen: seed}
   in { currentSeed: gen
      , nextPiece: val
      , currentGame: {piece: dummy, board: emptyBoard}}

scene :: Int -> { w :: Number, h :: Number } -> Behavior Drawing
scene seed { w, h } = pure background 
      <> map renderState state
 where
  background :: Drawing
  background = filled (fillColor $ rgb 0 0 0) (rectangle 0.0 0.0 w h)

  state :: Behavior State
  state = unfold (\code st -> 
    if code == "trigger" then
      let {val, gen} = random {val: dummy, gen: dummy} 
                              {val: dummy, gen: st.currentSeed}
          g = st.currentGame
          p' = moveDown g.piece
      in if floorCollision p' || boardCollision p' g.board 
         then 
          { currentSeed: gen
           , nextPiece: val
           , currentGame: { piece: st.nextPiece 
                          , board: boardHandler $ addToBoard g.piece g.board}}
          else st{currentGame{piece = p'}}
    else st {currentGame = refreshGame code st.currentGame}  
    ) (down `alt`(const "trigger" <$> interval 1000)) $ initialState seed

main :: Effect Unit
main = do
  mcanvas <- getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- getContext2D canvas
  _ <- setCanvasWidth canvas 320.0
  _ <- setCanvasHeight canvas 512.0
  w <- getCanvasWidth canvas
  h <- getCanvasHeight canvas
  seed <- randomInt 1 32767
  _ <- animate (scene seed { w, h }) (render ctx)
  pure unit
