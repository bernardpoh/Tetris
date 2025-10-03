
module Piece where

import Config (height, width)
import Data.Maybe

type Block = Maybe Shape

emptyRow::[Block]
emptyRow = replicate width Nothing

emptyGrid::[[Block]]
emptyGrid = replicate height $ replicate width Nothing

showBlock::Block->Char
showBlock Nothing = '.'
showBlock (Just s) = case s of
  S -> '#'
  Z -> '0'
  L -> '*'
  J -> '='
  T -> 'O'
  I -> 'o'
  O -> '+'

showBoard::[[Block]] -> String
showBoard = unlines . reverse . (map . map) showBlock

type Point = (Int, Int)
data Shape = S | Z | L | J | T | I | O deriving (Show, Enum, Eq, Bounded)
data Rotation = North | East | South | West deriving (Show, Enum, Eq)
data Piece = Piece Shape Rotation Point deriving Show

right :: Rotation -> Rotation
left :: Rotation -> Rotation
right r = toEnum ((fromEnum r + 1) `mod` 4)
left r = toEnum ((fromEnum r - 1) `mod` 4)

rot90::Point->Point
rot90 (x,y) = (-y,x)

offset::[Point]->Point->[Point]
offset ps (x0,y0) = [(x0 + x1, y0 + y1) | (x1,y1) <- ps]

getShape::Shape->Rotation->[Point]
getShape s d = case s of
  O -> shapePoints O
  I -> case d of
    North -> shapePoints s
    East -> (rot90) `map` shapePoints I
    South -> shapePoints s
    West -> (rot90) `map` shapePoints I
  _ -> case d of
    North -> shapePoints s
    East -> rot90 `map` shapePoints s
    South -> (rot90 . rot90) `map` shapePoints s
    West -> (rot90 . rot90 . rot90) `map` shapePoints s

asPoints::Piece->[Point]
asPoints piece = ps
  where
    Piece shape rotation centre = piece
    points = getShape shape rotation
    ps = points `offset` centre

checkPiece::Piece->[[Block]]->Bool
checkPiece piece grid
  = not (any checkPoint points)
  where
    points = asPoints piece
    checkPoint (x, y) =
        x < 0 || x >= width || y < 0 || y >= height || isOccupied (x, y)
    isOccupied (x, y) = isJust $ grid !! y !! x

putPiece::Piece->[[Block]]->[[Block]]
putPiece piece board =
  putBlocks ps block board
  where
    ps = asPoints piece
    Piece shape _ _ = piece
    block = Just shape

putBlocks::[Point]->Block->[[Block]]->[[Block]]
putBlocks ps newBlock grid =
  do 
    (y, row) <- zip [0..] grid
    return (
      do
        (x, currentBlock) <- zip [0..] row
        return (if elem (x,y) ps then newBlock else currentBlock))

shapePoints::Shape->[Point]
shapePoints O = [(0,1), (1,1),
                 (0,0), (1,0)]

shapePoints I = [(0,3),
                 (0,2),
                 (0,1),
                 (0,0)]                  `offset` (0,-1)

shapePoints J = [(1,2),
                 (1,1),
          (0,0), (1,0)]                  `offset`(-1,-1)

shapePoints L = [(0,2),
                 (0,1),
                 (0,0), (1,0)]           `offset`(0,-1)

shapePoints S = [(1,1), (2,1),
          (0,0), (1,0)]                  `offset`(-1,-1)

shapePoints T = [(0,1), (1,1), (2,1),
                        (1,0)]           `offset`(-1,-1)

shapePoints Z = [(0, 1), (1, 1),
                         (1, 0), (2, 0)] `offset`(-1,-1)