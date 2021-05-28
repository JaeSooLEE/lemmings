
module Mouse where

import SDL

import Data.Map
import qualified Data.Map as Map

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S
import Lemmings
import Coordinates
import SDL.Vect
import Data.Maybe
import qualified Data.Maybe as Maybe
import Moteur


handleEventsMouse :: [Event] -> GameState -> Maybe Int -> Maybe Int
handleEventsMouse events gm slem = case Prelude.foldr (\x y -> handleEventMouse x gm)  Nothing events of
--handleEventsMouse events gm slem = case Prelude.foldr (handleEventMouse x gm)  Nothing events of
  Nothing -> slem
  Just i -> Just i

handleShow :: [Event] -> GameState -> Maybe Int -> String
handleShow events gm slem = case Prelude.foldr (\x y -> handleEventMouse x gm)  Nothing events of
--handleEventsMouse events gm slem = case Prelude.foldr (handleEventMouse x gm)  Nothing events of
  Nothing -> ""
  Just i -> show i

handleShow2 ::[Event] -> GameState -> Maybe (Int, Int)
handleShow2 events gm = Prelude.foldr (\x y -> handleEventMousePrint x gm) Nothing events



handleEventMouse ::Event -> GameState -> Maybe Int
handleEventMouse ev gm = case eventPayload ev of
  MouseButtonEvent (MouseButtonEventData _ m _ _ _ (P (V2 x y))) -> Map.foldlWithKey lemming_clicked Nothing (ensLemmings gm) where
    lemming_clicked mi i l@(Lemming {position = (C xg yg)})
      |Maybe.isJust mi == True = mi
      |otherwise = if (m == Pressed) && (fromIntegral x) - (xg * 50) < 110 && (fromIntegral x) - (xg * 50) > (-10) && (fromIntegral y) - (yg * 50) < 150 && (fromIntegral y) - (yg * 50) > (-150)
        then (Just i) else Nothing
  _ -> Nothing

handleEventMousePrint ::Event -> GameState -> Maybe (Int, Int)
handleEventMousePrint ev gm = case eventPayload ev of
  MouseButtonEvent (MouseButtonEventData _ m _ _ _ (P (V2 x y))) -> if (m == Pressed) then (Just (fromIntegral x, fromIntegral y)) else Nothing
  _ -> Nothing
