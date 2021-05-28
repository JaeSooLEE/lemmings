{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Data.Maybe
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Lemmings
import Coordinates
import Niveau

import Data.Map
import qualified Data.Map as Map

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import Moteur
import qualified Moteur as Mot


import Mouse
import qualified Mouse as M




import qualified Debug.Trace as T

--import Model (GameState)
--import qualified Model as M

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1000 1000)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')


loadTerre :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadTerre rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/terre.bmp" (TextureId "Terre") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Terre") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Terre") sprite smap
  return (tmap', smap')

loadMetal :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMetal rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/metal.bmp" (TextureId "Metal") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Metal") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Metal") sprite smap
  return (tmap', smap')

loadEntree :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadEntree rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/entree.bmp" (TextureId "Entree") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Entree") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Entree") sprite smap
  return (tmap', smap')

loadSortie :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadSortie rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/sortie.bmp" (TextureId "Sortie") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "Sortie") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "Sortie") sprite smap
  return (tmap', smap')

loadMarcheurGauche :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMarcheurGauche rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/marcheurg.bmp" (TextureId "marcheurg") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "marcheurg") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "marcheurg") sprite smap
  return (tmap', smap')

loadMarcheurDroite :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMarcheurDroite rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/marcheurd.bmp" (TextureId "marcheurd") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "marcheurd") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "marcheurd") sprite smap
  return (tmap', smap')

loadPoseurGauche :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPoseurGauche rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/poseurg.bmp" (TextureId "poseurg") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "poseurg") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "poseurg") sprite smap
  return (tmap', smap')

loadPoseurDroite :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPoseurDroite rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/poseurd.bmp" (TextureId "poseurd") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "poseurd") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "poseurd") sprite smap
  return (tmap', smap')


loadTombeur :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadTombeur rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/tombeur.bmp" (TextureId "tombeur") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "tombeur") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "tombeur") sprite smap
  return (tmap', smap')

loadMort :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadMort rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/mort.bmp" (TextureId "mort") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "mort") (S.mkArea 0 0 100 50)
  let smap' = SM.addSprite (SpriteId "mort") sprite smap
  return (tmap', smap')

loadCreuseur :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadCreuseur rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/creuseur.bmp" (TextureId "creuseur") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "creuseur") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "creuseur") sprite smap
  return (tmap', smap')

loadStoppeur :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadStoppeur rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/stoppeur.bmp" (TextureId "stoppeur") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "stoppeur") (S.mkArea 0 0 50 100)
  let smap' = SM.addSprite (SpriteId "stoppeur") sprite smap
  return (tmap', smap')


loadArrow :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadArrow rdr tmap smap = do
  tmap' <- TM.loadTexture rdr "assets/arrow.bmp" (TextureId "arrow") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "arrow") (S.mkArea 0 0 50 50)
  let smap' = SM.addSprite (SpriteId "arrow") sprite smap
  return (tmap', smap')


displayLemming :: Renderer -> TextureMap -> SpriteMap -> Lemming -> IO ()
displayLemming rnd tm sm l@Lemming{position = (C x y), role = r, niveau = n, direction = d,  chute = c}
  | r == Marcheur && d == G = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "marcheurg") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  | r == Marcheur && d == D = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "marcheurd") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  | r == Tombeur = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "tombeur") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  | r == Mort = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "mort") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral (y * 50)))
  | r == Creuseur = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "creuseur") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  | r == Stoppeur = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "stoppeur") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  | r == Poseur && d == G = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "poseurg") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  | r == Poseur && d == D = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "poseurd") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 1) * 50)))
  |otherwise = return ()

displayCase :: Renderer -> TextureMap -> SpriteMap -> Case -> Coord -> IO ()
displayCase rnd tm sm cse (C x y)
  |cse == T = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "Terre") sm)
                               (fromIntegral (x * 50))
                               (fromIntegral (y * 50)))
  |cse == M = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "Metal") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral (y * 50)))
  |cse == Niveau.E = S.displaySprite rnd  tm (S.moveTo (SM.fetchSprite (SpriteId "Entree") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral (y * 50)))
  |cse == S = S.displaySprite rnd tm (S.moveTo (SM.fetchSprite (SpriteId "Sortie") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral (y * 50)))
  |otherwise = return ()


displayCases :: Renderer -> TextureMap -> SpriteMap -> Int -> Int -> Niveau -> IO ()
displayCases rend tm sm x y n
  | y >= (hNiveau n) = return ()
  | x >= (lNiveau n) = displayCases rend tm sm 0 (y + 1) n
  | otherwise = do
      displayCase rend tm sm (Maybe.fromJust (Map.lookup (C x y) (fromGrille (casesNiveau n)))) (C x y)
      displayCases rend tm sm (x + 1) y n


displayLemmings :: Renderer -> TextureMap -> SpriteMap -> Int -> Map Int Lemming -> IO ()
displayLemmings rend tm sm idt mapLem = case Map.lookup idt mapLem of
  Nothing -> return ()
  Just l -> do
    displayLemming rend tm sm l
    displayLemmings rend tm sm (idt + 1) mapLem

displayArrow :: Renderer -> TextureMap -> SpriteMap -> Maybe Int -> Moteur.GameState -> IO ()
displayArrow rnd tm sm i gs
  |Maybe.isJust i == True = let (C x y) = (position $ Maybe.fromJust $ Map.lookup (Maybe.fromJust i) (ensLemmings gs)) in
    S.displaySprite rnd  tm (S.moveTo (SM.fetchSprite (SpriteId "arrow") sm)
                                 (fromIntegral (x * 50))
                                 (fromIntegral ((y - 2) * 50)))
  |otherwise = return ()

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 1000 1000 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap0, smap0) <- loadBackground renderer "assets/background2.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap1, smap1) <- loadTerre renderer tmap0 smap0
  (tmap2, smap2) <- loadMetal renderer tmap1 smap1
  (tmap3, smap3) <- loadEntree renderer tmap2 smap2
  (tmap4, smap4) <- loadSortie renderer tmap3 smap3
  (tmap5, smap5) <- loadMarcheurDroite renderer tmap4 smap4
  (tmap6, smap6) <- loadMarcheurGauche renderer tmap5 smap5
  (tmap7, smap7) <- loadTombeur renderer tmap6 smap6
  (tmap8, smap8) <- loadCreuseur renderer tmap7 smap7
  (tmap9, smap9) <- loadArrow renderer tmap8 smap8
  (tmap10, smap10) <- loadStoppeur renderer tmap9 smap9
  (tmap11, smap11) <- loadPoseurDroite renderer tmap10 smap10
  (tmap12, smap12) <- loadPoseurGauche renderer tmap11 smap11
  (tmap, smap) <- loadMort renderer tmap12 smap12

  niveau1 <- initNiveau "assets/niveau1.txt"
  niveau2 <- initNiveau "assets/niveau2.txt"
  niveau3 <- initNiveau "assets/niveau3.txt"
  putStrLn $ show (lNiveau niveau1)
  putStrLn $ show niveau1
  -- initialisation de l'état du jeu
  let gameState = Mot.initGameState niveau1
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameState Nothing 1

  let gameState = Mot.initGameState niveau2

  gameLoop 60 renderer tmap smap kbd gameState Nothing 1

  let gameState = Mot.initGameState niveau3

  gameLoop 60 renderer tmap smap kbd gameState Nothing 1

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Moteur.GameState -> Maybe Int -> Int-> IO ()
gameLoop frameRate renderer tmap smap kbd gameState selectedLem stepNum= do
  startTime <- time
  events <- pollEvents
  let sLem = M.handleEventsMouse events gameState selectedLem
  let kbd' = K.handleEvents events kbd

  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  let sLem = M.handleEventsMouse events gameState selectedLem
  displayArrow renderer tmap smap sLem gameState
  --- display lemmings
  displayLemmings renderer tmap smap 0 (ensLemmings gameState)
  let sLem = M.handleEventsMouse events gameState selectedLem
  --- display cases
  displayCases renderer tmap smap 0 0 (niveauS gameState)
  let sLem = M.handleEventsMouse events gameState selectedLem
  let i = handleShow events gameState selectedLem
  --putStrLn i

  --detectingDelay :: Int ->

  --S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "Metal") smap) (fromIntegral 200) (fromIntegral 200))
  --putStrLn $ show $ Maybe.fromJust $ Map.lookup (C 1 0) (casesNiveau (niveauS gameState))
  --displayCase renderer tmap smap (Maybe.fromJust (Map.lookup (C 0 0) (casesNiveau (niveauS gameState)))) (C 0 0)
  --putStrLn $ show $ noLemming gameState

  let sLem = M.handleEventsMouse events gameState selectedLem

  present renderer
  let sLem = M.handleEventsMouse events gameState selectedLem
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)




  threadDelay $ delayTime * 2000 -- microseconds
  let sLem = M.handleEventsMouse events gameState selectedLem






  endTime <- time
  let sLem = M.handleEventsMouse events gameState selectedLem
  let deltaTime = endTime - startTime
  let sLem = M.handleEventsMouse events gameState selectedLem
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  -- let gameState' = moteurJeu gameState kbd' deltaTime
  let gameState' = gameStep gameState kbd' sLem deltaTime stepNum
  ---
  unless (K.keypressed KeycodeEscape kbd' || defaite gameState' 6 || victoire gameState' 6) (gameLoop frameRate renderer tmap smap kbd' gameState' sLem (stepNum + 1))
