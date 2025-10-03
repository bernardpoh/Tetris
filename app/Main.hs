{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Terminal.Game
import System.Random (newStdGen, randomR)
import Piece (Piece(..), Shape(..), Rotation(..), Block, Point, left, right, checkPiece, putPiece, emptyGrid, emptyRow, showBoard)
import Config

main :: IO ()
main = do
  seed <- newStdGen
  playGame (myGame seed)

myGame :: StdGen -> Game GameState ()
myGame seed = Game
  { gTPS = tickRate -- Game ticks per second
  , gInitState = initialState seed -- Initial game state
  , gLogicFunction = gameLogic -- Logic function for game
  , gDrawFunction = gameDraw   -- Drawing function for game
  }

gameDraw :: GEnv -> GameState -> Plane
gameDraw _ g = stringPlane $ text
  where
    text = showGame g

gameLogic :: GEnv -> GameState -> Event -> Either () GameState
gameLogic _ g (KeyPress k) = keyPressedEvent k g
gameLogic _ g (Tick) = tickEvent g 

tickEvent::GameState-> Either () GameState
tickEvent g = Right $ onTickEvent g

keyPressedEvent::Char-> GameState-> Either () GameState
keyPressedEvent 'z' g = Right $ turnRight g
keyPressedEvent 'x' g = Right $ turnLeft g
keyPressedEvent 'q' _ = Left ()
-- it turns out that when you press the left arrow button, it does not map to any specific character
-- instead it maps to the succession of characters ['\ESC', '[', 'D']
-- Also, it can detect capital letters and characters, which is affected by caps lock and shift
-- The caps lock and shift keys cannot be detected by themselves
-- Also, right click pastes whatever list of characters is in your clickboard
-- its very weird
-- For our program we will use 'D' to detect left and so on for all the other arrow keys
keyPressedEvent 'D' g = Right $ moveLeft g -- left arrow
keyPressedEvent 'C' g = Right $ moveRight g -- right arrow
keyPressedEvent 'A' g = Right $ turnRight g -- up arrow
keyPressedEvent 'B' g = Right $ moveDown g -- down arrow
keyPressedEvent ' ' g = Right $ hardDrop g
keyPressedEvent _ g = Right g

data PieceState = Falling Piece Int | Locking Piece Int | Waiting Int | ClearingLines Int deriving Show

data GameState = GameState {
    grid :: [[Block]]
    ,pieceState :: PieceState
    ,shapes :: [Shape]
    ,fallAmount :: Int
    ,fallDelay :: Int
    ,lockDelay :: Int
    ,waitDelay :: Int
    ,lineClearDelay :: Int
  }

initialState :: StdGen -> GameState
initialState seed = GameState {
  grid = emptyGrid
  ,pieceState = Falling piece 0
  ,shapes = getPieces seed
  ,fallAmount = 1
  ,fallDelay = 60
  ,lockDelay = 60
  ,waitDelay = 30
  ,lineClearDelay = 41
  }
  where
    piece = Piece I North (5,22)
    getPieces gen = randomShape : getPieces newGen
      where
        (randomNum, newGen) = randomR (0, 6) gen
        randomShape = toEnum randomNum

padding::String
padding = "                                     "

showGame :: GameState -> String
showGame GameState {..} =
  showBoard newGrid
  ++ "pieceState" ++ (show pieceState) ++ padding ++ "\n"
  where
    newGrid = case pieceState of
      Waiting  _ -> grid
      Falling piece _ -> putPiece piece grid
      Locking piece _ -> putPiece piece grid
      ClearingLines n -> animateClearingLines n
    
    animateClearingLines::Int->[[Block]]
    animateClearingLines _ = [if isRowCleared row then emptyRow else row | row <- grid]

move::(Piece->Piece)->GameState->GameState
move f g@(GameState {..}) = g {pieceState=newPieceState}
  where
    newPieceState = case pieceState of
      Waiting n -> Waiting n
      ClearingLines n -> ClearingLines n
      Falling piece n -> tryMove piece n
      Locking piece n -> tryMove piece n
    
    tryMove::Piece->Int->PieceState
    tryMove piece n = newPieceState
      where
        movedPiece = f piece
        isValid = checkPiece movedPiece grid
        newPiece = if isValid then movedPiece else piece
        fallenPiece = moveCentre (0,-1) newPiece
        canFall = checkPiece fallenPiece grid
        newPieceState = if canFall then Falling newPiece n else Locking newPiece n

rotate::(Rotation->Rotation)->Piece->Piece
rotate f piece = Piece shape (f rotation) centre
  where
    Piece shape rotation centre = piece

moveCentre::Point->Piece->Piece
moveCentre (x1,y1) piece = Piece shape rotation (x0+x1,y0+y1)
  where
    Piece shape rotation (x0,y0) = piece

moveRight::GameState->GameState
moveRight = move $ moveCentre (1, 0)
moveLeft::GameState->GameState
moveLeft = move $ moveCentre (-1, 0)
moveDown::GameState->GameState
moveDown = move $ moveCentre (0, -1)

fall::Int->GameState->GameState
fall n = repeat n $ move $ moveCentre (0, -1)
    where
      repeat :: Int -> (a -> a) -> (a -> a)
      repeat 0 _ = id
      repeat n f = f . repeat (n - 1) f

hardDrop::GameState->GameState
hardDrop = fall 25

turnRight::GameState->GameState
turnRight = move $ rotate right

turnLeft::GameState->GameState
turnLeft = move $ rotate left

onTickEvent::GameState -> GameState
onTickEvent = checkPieceTimer . advancePieceTimer

advancePieceTimer::GameState->GameState
advancePieceTimer g@(GameState {..}) = g {pieceState=newPieceState}
  where
    newPieceState = case pieceState of
      Falling piece n -> Falling piece (n+1)
      Locking piece n -> Locking piece (n+1)
      Waiting n -> Waiting (n+1)
      ClearingLines n -> ClearingLines (n+1)

checkPieceTimer::GameState->GameState
checkPieceTimer g@(GameState {..})
  = case pieceState of
    Falling piece n | n >= fallDelay -> (fall fallAmount) (g {pieceState = Falling piece 0})
                    | otherwise -> g
    Locking piece n | n >= lockDelay -> landPiece $ g {grid = putPiece piece grid}
                    | otherwise -> g
    Waiting n       | n >= waitDelay -> nextPiece $ g 
                    | otherwise -> g
    ClearingLines n | n >= waitDelay -> nextPiece $ g {grid = clearLines grid}
                    | otherwise -> g

clearLines::[[Block]]->[[Block]]
clearLines grid = take height $ filter (not . isRowCleared) grid ++ repeat emptyRow

isRowCleared::[Block]->Bool
isRowCleared row = all hasBlock row
  where
    hasBlock::Block->Bool
    hasBlock (Just _) = True
    hasBlock Nothing = False

landPiece::GameState->GameState
landPiece g@(GameState {..}) = if any isRowCleared grid then g {pieceState = ClearingLines 0} else g {pieceState = Waiting 0}

nextPiece::GameState->GameState
nextPiece g@(GameState {..}) = g {shapes=newShapes, pieceState=newPieceState}
  where
    (s,newShapes) = case shapes of
      (x:xs) -> (x,xs)
      [] -> error "ran out of shapes"
    newPiece = Piece s North (5,22)
    newPieceState = Falling newPiece 0
