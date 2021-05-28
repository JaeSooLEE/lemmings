module Moteur where
import SDL
import Coordinates
import Niveau
import Lemmings
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Maybe as M

import Keyboard (Keyboard)
import qualified Keyboard as K



data GameState = GameState{niveauS :: Niveau, ensLemmings :: Map Int Lemming, lemmingId :: Int}
data Fin = Victoire | Defaite

--propriété sur GameState
gameState_inv :: GameState -> Bool
gameState_inv (GameState n ens i) = prop_NiveauCorrect n && Map.foldl (\b x -> if b == False then False else lemming_inv x) True ens



initGameState :: Niveau -> GameState
initGameState n = GameState n Map.empty 0




ajouterLemming :: GameState -> GameState
ajouterLemming (GameState n ens i) =
  GameState n (Map.insert i (Lemming (bougeCoord B (M.fromJust (entreeNiveau n))) Tombeur n D 0) ens) (i + 1)


--l'ajout du lemming a bien été fait (et on a ajouté le bon lemming)
post_ajout_lemming :: GameState -> Bool
post_ajout_lemming (GameState n ens i) = let e = M.fromJust $ entreeNiveau n in
  case Map.lookup (i - 1) ens of
    Nothing -> False
    Just (Lemming c r niv d ch) -> c == (bougeCoord B e) && r == Tombeur && niv == n && d == D && ch == 0

--on vérifie que le lemming j existe avant de le manipuler dans le GameState
pre_manip_lemming :: GameState -> Int -> Bool
pre_manip_lemming (GameState n ens i) j = case Map.lookup j ens of
  Just l -> True
  _-> False



enleverLemming :: GameState -> Int -> GameState
enleverLemming (GameState n ens i) idt =
  GameState n (Map.delete idt ens) i

--on a bien enlevé le lemming j
post_suppr_lemming :: GameState -> Int -> Bool
post_suppr_lemming (GameState n ens i) j = case Map.lookup j ens of
  Nothing -> True
  _-> False



deplacerLemming :: GameState -> Int -> Coord -> GameState
deplacerLemming (GameState n ens i) idt crd =
  GameState n (deplHM idt crd ens) i where
    deplHM idty crds mapLem = case Map.lookup idty mapLem of
      Nothing -> mapLem
      Just l -> Map.insert idty (deplaceP l crds) mapLem

--on a bien déplacé le lemming j à l'emplacement voulu
post_deplacer_lemming :: GameState -> Coord -> Int -> Bool
post_deplacer_lemming (GameState n ens i) ct j = case Map.lookup j ens of
  Nothing -> False
  Just (Lemming c r niv d idy) -> if c == ct then True else False

bougerLemming :: GameState  -> Int -> Deplacement -> GameState
bougerLemming (GameState n ens i) idt dep =
  GameState n (bougeHM idt dep ens) i where
    bougeHM idty dpt mapLem = case Map.lookup idty mapLem of
      Nothing -> mapLem
      Just l -> Map.insert idty (bougeP l dpt) mapLem




installerStoppeur :: Int -> Role -> GameState -> Niveau
installerStoppeur i Stoppeur (GameState (Niveau h lg (Grille g)) ens idt) = case Map.lookup i ens of
  Just l -> Niveau h lg (Grille (Map.insert (bougeCoord H (position l)) L (Map.insert (position l) L g)))
  _ -> Niveau h lg (Grille g)


installerStoppeur i _ (GameState (Niveau h lg (Grille g)) ens idt) = case Map.lookup i ens of
  Just l -> Niveau h lg (Grille (Map.insert (bougeCoord H (position l)) V (Map.insert (position l) V g)))
  _ -> Niveau h lg (Grille g)

modifierLemming :: Int -> Role -> GameState -> GameState
modifierLemming idt rol (GameState n ens i) =
  GameState (installerStoppeur idt rol (GameState n ens i)) (modifyHM idt rol ens) i where
    modifyHM idty r mapLem = case Map.lookup idty mapLem of
      Nothing -> mapLem
      Just l -> Map.insert idty (changeRole l r) mapLem

--Le lemming j a été modifié et le Niveau modifié en conséquence
post_modifierLemming :: Int -> Role -> GameState -> Bool
post_modifierLemming j Stoppeur (GameState n ens i) =
  case Map.lookup j ens of
    Just (Lemming c r niv d ch) -> r == Stoppeur && case Map.lookup c (fromGrille (casesNiveau n)) of
      Just L -> True
      _-> False
      && case Map.lookup (bougeCoord H c) (fromGrille (casesNiveau n)) of
        Just L -> True
        _-> False
    _-> False

post_modifierLemming j rol (GameState n ens i) =
  case Map.lookup j ens of
    Just (Lemming c r niv d ch) -> r == rol && case Map.lookup c (fromGrille (casesNiveau n)) of
      Just V -> True
      _-> False
      && case Map.lookup (bougeCoord H c) (fromGrille (casesNiveau n)) of
        Just V -> True
        _-> False
    _-> False



stateChanger :: (Niveau, Map Int Lemming) -> Int -> Lemming -> (Niveau, Map Int Lemming)
stateChanger (n, hm) i (Lemming pos r niv dir c) =
  let lem = tourLemming (Lemming pos r n dir c) in
    ((niveau lem), Map.insert i lem hm)

allLemmingsTurn :: Niveau -> Map Int Lemming -> (Niveau, Map Int Lemming)
allLemmingsTurn n hs = Map.foldlWithKey stateChanger (n, Map.empty) hs


lemAllDead :: Bool -> Lemming -> Bool
lemAllDead False _ = False
lemAllDead True l
  |(role l) == Mort = True
  |otherwise = False

numberDeath :: Int -> Lemming -> Int
numberDeath i l
  |(role l) == Mort = i + 1
  |otherwise = i

numberSuccess :: Int -> Lemming -> Int
numberSuccess i l
  |(role l) == Fini = i + 1
  |otherwise = i


defaite :: GameState -> Int -> Bool
defaite gs target = let count = Map.foldl numberDeath 0 (ensLemmings gs) in
  count > (10 - target)

victoire :: GameState -> Int -> Bool
victoire gs target = let count = Map.foldl numberSuccess 0 (ensLemmings gs) in
  count >= target


noLemming :: GameState -> Bool
noLemming gs = (Map.lookup 0 (ensLemmings gs)) == Nothing


moteurJeu :: GameState -> Int -> GameState
moteurJeu gs i
  | rem i 50 == 0 && (lemmingId gs) < 10  = let (niv, hs) = Map.foldlWithKey stateChanger ((niveauS gs), Map.empty) (ensLemmings gs) in
    ajouterLemming (GameState niv hs (lemmingId gs))
  | rem i 10 == 0 = let (niv, hs) = Map.foldlWithKey stateChanger ((niveauS gs), Map.empty) (ensLemmings gs) in
    GameState niv hs (lemmingId gs)
  | otherwise = gs

--vérifie le GameState après un tour de jeu.
post_moteurJeu :: GameState -> Bool
post_moteurJeu gs = gameState_inv gs


gameStep :: RealFrac a => GameState -> Keyboard -> Maybe Int -> a -> Int -> GameState
gameStep gstate kbd selectedLem deltaTime stepNum =
  let lem = M.fromJust $ Map.lookup (M.fromJust selectedLem) (ensLemmings gstate) in
  let modif = (if K.keypressed KeycodeQ kbd && (role lem) /= Mort && (role lem) /= Tombeur
               then (modifierLemming (M.fromJust selectedLem) Creuseur) else id)
              .
              (if K.keypressed KeycodeD kbd && (role lem) /= Mort && (role lem) /= Tombeur
               then (modifierLemming (M.fromJust selectedLem) Poseur) else id)
              .
              (if K.keypressed KeycodeZ kbd && (role lem) /= Mort && (role lem) /= Tombeur
               then (modifierLemming (M.fromJust selectedLem) Creuseur) else id)
                .
              (if K.keypressed KeycodeS kbd && (role lem) /= Mort && (role lem) /= Tombeur
               then (modifierLemming (M.fromJust selectedLem) Stoppeur) else id)

  in moteurJeu (modif gstate) stepNum
