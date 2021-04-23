
module Mouse where

import SDL



import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

import SDL.Vect

import Model 




handleEventsMouse :: [Event] -> GameState -> String
handleEventsMouse events gm = if (foldr (\x y -> y || handleEventMouse x gm)  False events) == True  then "mouse click gg wp\n" else ""

handleEventMouse :: Event -> GameState -> Bool
handleEventMouse ev gm@(GameState {persoX = xg, persoY = yg}) = case eventPayload ev of
  MouseButtonEvent (MouseButtonEventData _ m _ _ _ (P (V2 x y))) -> if m == Pressed && (fromIntegral x) - xg < 100 && (fromIntegral x) - xg > 0 && (fromIntegral y) - yg < 100 && (fromIntegral y) - yg > 0 then True else False
  _ -> False
