{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Terminal.Game
import System.Random (newStdGen, randomR)
import Config
import Piece (Piece(..), Shape(..), Rotation(..), Block(..), Point, left, right, checkPiece, putPiece, emptyGrid, showBoard)

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

gameLogic :: GEnv -> GameState -> Event -> Either () GameState
gameLogic _ _ (KeyPress 'q') = Left ()
gameLogic _ g (KeyPress k) = keyPressedEvent k g
gameLogic _ g (Tick) = tickEvent g 

gameDraw :: GEnv -> GameState -> Plane
gameDraw _ g = stringPlane $ text
  where
    text = showGame g

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

data PieceState = Falling Piece Int | Locking Piece Int | Waiting Int  deriving Show

data GameState = GameState {
    grid :: [[Block]]
    ,pieceState :: PieceState
    ,shapes :: [Shape]
    ,fallDelay :: Int
    ,lockDelay :: Int
    ,waitDelay :: Int
  }

initialState :: StdGen -> GameState
initialState seed = GameState {
  grid = emptyGrid
  ,pieceState = Falling piece 0
  ,shapes = getPieces seed
  ,fallDelay = 60
  ,lockDelay = 30
  ,waitDelay = 30
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

move::(Piece->Piece)->GameState->GameState
move f g@(GameState {..}) = checkFalling moveResult
  where
    newPieceState = case pieceState of
      Waiting n -> Waiting n
      Falling piece n -> Falling (tryMove piece) n
      Locking piece n -> Locking (tryMove piece) n
    
    moveResult = g {pieceState=newPieceState}
    
    tryMove::Piece->Piece
    tryMove piece = if isValid then newPiece else piece
      where
        newPiece = f piece
        isValid = checkPiece newPiece grid

    checkFalling::GameState->GameState
    checkFalling g@(GameState {..}) = g {pieceState=newPieceState}
      where
        newPieceState = case pieceState of
          Falling p n | canFall p -> Falling p n
                      | otherwise -> Locking p n
          Locking p n | canFall p -> Falling p n
                      | otherwise -> Locking p n
          Waiting n -> Waiting n
    
    canFall::Piece->Bool
    canFall piece = checkPiece newPiece grid
      where
        newPiece = moveCentre (0,-1) piece
    

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

hardDrop::GameState->GameState
hardDrop = move $ moveCentre (0, -1)

turnRight::GameState->GameState
turnRight = move $ rotate right

turnLeft::GameState->GameState
turnLeft = move $ rotate left

tickEvent::GameState -> Either () GameState
tickEvent = Right . checkPieceTimer . advancePieceTimer

advancePieceTimer::GameState->GameState
advancePieceTimer g@(GameState {..}) = g {pieceState=newPieceState}
  where
    newPieceState = case pieceState of
      Falling piece n -> Falling piece (n+1)
      Locking piece n -> Locking piece (n+1)
      Waiting n -> Waiting (n+1)

checkPieceTimer::GameState->GameState
checkPieceTimer g@(GameState {..})
  = case pieceState of
    Falling piece n | n >= fallDelay -> move (moveCentre (0,-1)) (g {pieceState = Falling piece 0})
                    | otherwise -> g
    Locking piece n | n >= lockDelay-> g {pieceState = Waiting 0, grid = putPiece piece grid}
                    | otherwise -> g
    Waiting n       | n >= waitDelay -> nextPiece $ g
                    | otherwise -> g

nextPiece::GameState->GameState
nextPiece g@(GameState {..}) = g{shapes=newShapes, pieceState=Falling newPiece 0}
  where
    (s,newShapes) = case shapes of
      (x:xs) -> (x,xs)
      [] -> error "ran out of shapes"
    newPiece = Piece s North (5,22)
