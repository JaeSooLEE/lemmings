
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)

  
initGameState :: GameState
initGameState = GameState 200 300 4

moveLeft :: GameState -> GameState
moveLeft gs@(GameState {persoX = x}) = gs{persoX = x - 1}

moveRight :: GameState -> GameState
moveRight gs@(GameState {persoX = x}) = gs{persoX = x + 1}
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState {persoY = y}) = gs{persoY = y + 1}

moveDown :: GameState -> GameState
moveDown gs@(GameState {persoY = y}) = gs{persoY = y - 1}

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeQ kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeD kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeZ kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeS kbd
               then moveDown else id)

  in modif gstate
