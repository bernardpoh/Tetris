{-# LANGUAGE OverloadedStrings #-}

import Terminal.Game

spaces::String
spaces = "                      "

-- Define the game state
data GameState = GameState
  { counter :: Integer  -- Counter variable that will be incremented
  } deriving (Show)

-- Initial game state
initialState :: GameState
initialState = GameState { counter = 0 }

-- Logic function to handle game events
gameLogic :: GEnv -> GameState -> Event -> Either () GameState
gameLogic _ (GameState c) (KeyPress 'a') = Right $ GameState (0)
gameLogic _ (GameState c) (KeyPress 'q') = Left ()
gameLogic _ (GameState c) _ = Right $ GameState (c + 1)

-- Draw function to render the current game state
gameDraw :: GEnv -> GameState -> Plane
gameDraw _ (GameState c) = stringPlane $ "Counter: " ++ show c ++ spaces -- Display the counter value

-- Define the game
myGame :: Game GameState ()
myGame = Game
  { gTPS = 60                 -- Game ticks per second
  , gInitState = initialState -- Initial game state
  , gLogicFunction = gameLogic -- Logic function for game
  , gDrawFunction = gameDraw   -- Drawing function for game
  }

-- Main function to play the game
main :: IO ()
main = do
  playGame myGame  -- Start the game