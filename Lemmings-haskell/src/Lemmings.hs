module Lemmings where

import Coordinates
import Niveau
import Data.Maybe
import qualified Data.Maybe as M

data Role = Tombeur | Marcheur | Creuseur | Mort | Fini | Stoppeur | Poseur
  deriving (Eq, Show)

data Lemming = Lemming { position :: Coord, role :: Role, niveau :: Niveau, direction :: Deplacement, chute :: Int}
  deriving Eq


lemming_inv :: Lemming -> Bool
lemming_inv (Lemming (C x y) Stoppeur n d ch)
  |x < 0 || x >= (lNiveau n) || y < 0 || y >= (hNiveau n) = False
  |isLemming (C x y) n && isLemming (bougeCoord H (C x y)) n = True
  |otherwise = False

lemming_inv (Lemming (C x y) r n d ch)
  |x < 0 || x >= (lNiveau n) || y < 0 || y >= (hNiveau n) = False
  |passable (C x y) n && passable (bougeCoord H (C x y)) n = True
  |otherwise = False

instance Placable Lemming where
  coordP l = position l
  bougeP l d = Lemming{position = bougeCoord d (position l), role = (role l), niveau = (niveau l), direction = (direction l), chute = (chute l) }
  deplaceP l crd = Lemming{position = crd, role = (role l), niveau = (niveau l), direction = (direction l), chute = (chute l)}

changeRole :: Lemming -> Role -> Lemming
changeRole (Lemming crd r n d c) rle =
  Lemming crd rle n d 0


path_clear :: Lemming -> Bool
path_clear l
  | (direction l) == G = (passable (bougeCoord G (position l)) (niveau l)) && (passable (bougeCoord GH (position l)) (niveau l))
  | (direction l) == D = (passable (bougeCoord D (position l)) (niveau l)) && (passable (bougeCoord DH (position l)) (niveau l))
  |otherwise = False


path_stair :: Lemming -> Bool
path_stair l
  | (direction l) == G = (passable (bougeCoord GH (position l)) (niveau l)) && (passable (bougeCoord H (bougeCoord GH (position l))) (niveau l))
  | (direction l) == D = (passable (bougeCoord DH (position l)) (niveau l)) && (passable (bougeCoord H (bougeCoord DH (position l))) (niveau l))
  | otherwise = False

peut_creuser :: Lemming -> Bool
peut_creuser l@(Lemming c _ (Niveau x y grille) d ch) = creusable (bougeCoord B c) grille

peut_poser :: Lemming ->  Bool
peut_poser l@(Lemming {position = crd, niveau = (Niveau x y grille), direction = d}) = posable (bougeCoord d crd) grille


tourMarcheur :: Lemming -> Lemming
tourMarcheur l@Lemming{position = crd, role = r, niveau = n, direction = d,  chute = c}
  |crd == M.fromJust (sortieNiveau n) = (Lemming crd Fini n d c)
  |passable (bougeCoord B crd) n == True = Lemming{position = (bougeCoord B crd), role = Tombeur, niveau = n, direction = d, chute = 1}
  |path_clear l = Lemming{position = (bougeCoord d crd), role = Marcheur, niveau = n, direction = d, chute = 0}
  |path_stair l && d == G = Lemming{position = (bougeCoord GH crd), role = Marcheur, niveau = n, direction = d, chute = 0}
  |path_stair l && d == D = Lemming{position = (bougeCoord DH crd), role = Marcheur, niveau = n, direction = d, chute = 0}
  |d == G = Lemming{position = crd, role = Marcheur, niveau = n, direction = D,  chute = 0}
  |otherwise = Lemming{position = crd, role = Marcheur, niveau = n, direction = G,  chute = 0}

tourTombeur :: Lemming -> Lemming
tourTombeur l@Lemming{position = crd, role = r, niveau = n, direction = d, chute = c}
  |dure (bougeCoord B crd) n == False = Lemming{position = (bougeCoord B crd), role = Tombeur, niveau = n, direction = d, chute = c + 1}
  |c >= 8 = Lemming{position = crd, role = Mort, niveau = n, direction = d, chute = c}
  |otherwise = Lemming{position = crd, role = Marcheur, niveau = n, direction = d, chute = 0}

tourCreuseur :: Lemming -> Lemming
tourCreuseur l@Lemming{position = crd, role = r, niveau = n, direction = d, chute = c}
--  |peut_creuser l == True = Lemming (bougeCoord B crd) Creuseur (creuse (bougeCoord GB crd) $ creuse (bougeCoord DB crd) $ creuse (bougeCoord B crd) (casesNiveau n)) d c
  |peut_creuser l == True = Lemming (bougeCoord B crd) Creuseur (Niveau (hNiveau n) (lNiveau n)((creuse (bougeCoord B crd) $ creuse (bougeCoord DB crd) $ creuse (bougeCoord GB crd) (casesNiveau n)))) d c
  |otherwise = Lemming crd Marcheur n d 0

tourPoseur :: Lemming -> Lemming
tourPoseur l@Lemming{position = crd, role = r, niveau = n, direction = d, chute = c}
  |c >= 6 = Lemming crd Marcheur n d 0
  |peut_poser l == True = Lemming crd Poseur (Niveau (hNiveau n) (lNiveau n) (pose (bougeCoord d crd) (casesNiveau n))) d (c + 1)
  |path_stair l && d == G = Lemming{position = (bougeCoord GH crd), role = Poseur, niveau = n, direction = d, chute = c}
  |path_stair l && d == D = Lemming{position = (bougeCoord DH crd), role = Poseur, niveau = n, direction = d, chute = c}
  |otherwise = Lemming crd Marcheur n d 0

tourLemming :: Lemming -> Lemming
tourLemming l
  |(role l) == Marcheur = tourMarcheur l
  |(role l) == Tombeur = tourTombeur l
  |(role l) == Creuseur = tourCreuseur l
  |(role l) == Poseur = tourPoseur l
  |otherwise = l
