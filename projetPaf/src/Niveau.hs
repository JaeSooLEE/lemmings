module Niveau where


import Data.Map (Map)
import qualified Data.Map as Map

import Data.List
import qualified Data.List as List

import Coordinates

import Data.Maybe
import qualified Data.Maybe as M

data Position = Pos Grille Coord Deplacement
  deriving Eq

data Case = V | T | M | E | S | L
  deriving Eq

data Niveau = Niveau { hNiveau :: Int,
                       lNiveau :: Int,
                       casesNiveau :: Grille}
                       deriving Eq

class HasBoxes h where
  getBox :: Int -> Int -> h -> Case
  addBox :: Int -> Int -> Case -> h -> h
  bempty :: h
  hauteur :: h -> Int
  largeur :: h -> Int

data Grille = Grille (Map Coord Case)
  deriving Eq

fromGrille :: Grille -> Map Coord Case
fromGrille (Grille g) = g

instance HasBoxes Grille where
  getBox x y (Grille m) = M.fromJust $ Map.lookup (C x y) m
  addBox x y c (Grille m)= Grille (Map.insert (C x y) c m)
  bempty = Grille Map.empty
  hauteur (Grille m) = (Map.foldlWithKey (\h (C x y) _ -> if x > h then x else h) 0 m ) + 1
  largeur (Grille m) = (Map.foldlWithKey (\l (C x y) _-> if y > l then y else l) 0 m ) + 1



initNiveau :: FilePath -> IO (Niveau)
initNiveau f = do
    file <- readFile f
    return (read file)


compteur :: (Int, Int) -> Case -> (Int, Int)
compteur (a, b) c
  |c == E = (a + 1, b)
  |c == S = (a, b + 1)
  |otherwise = (a, b)


compteur_EntreeSortie :: Niveau -> (Int, Int)
compteur_EntreeSortie (Niveau x y (Grille ens)) = foldl compteur (0, 0) ens


prop_NiveauUniqueES :: Niveau -> Bool
prop_NiveauUniqueES n
  |compteur_EntreeSortie n == (1, 1) = True
  |otherwise = False


prop_FermetureMetal :: Niveau -> Bool
prop_FermetureMetal (Niveau l h (Grille ens)) = Map.foldlWithKey testBordure True ens where
  testBordure b (C x y) c
    |x == 0 || y == 0 || x == l - 1 || y == h - 1 = getBox x y (Grille ens) == M
    |otherwise = b

entreeNiveau :: Niveau -> Maybe Coord
entreeNiveau (Niveau l h (Grille ens)) = Map.foldlWithKey (searchEntree) Nothing ens where
  searchEntree c crd cse
        |cse == E = Just crd
        |otherwise = c

sortieNiveau :: Niveau -> Maybe Coord
sortieNiveau (Niveau l h (Grille ens)) = Map.foldlWithKey (searchSortie) Nothing ens where
  searchSortie c crd cse
        |cse == S = Just crd
        |otherwise = c

prop_EntreeSurVide :: Niveau -> Bool
prop_EntreeSurVide (Niveau l h (Grille ens)) = case entreeNiveau (Niveau l h (Grille ens)) of
    Nothing -> False
    Just (C x y) -> Map.lookup (C x (y + 1)) ens == Just V

    --entreeNiveau n in
    --Map.findWithDefault NA (C x (y + 1)) (casesNiveau n) == V

prop_SortieSurMetal :: Niveau -> Bool
prop_SortieSurMetal (Niveau l h (Grille ens)) = case sortieNiveau (Niveau l h (Grille ens)) of
    Nothing -> False
    Just (C x y) -> Map.lookup (C x (y + 1)) ens == Just M


coords_associated :: Coord -> Niveau -> Bool
coords_associated (C x y) (Niveau l h (Grille ens))
  | x >= l = coords_associated (C 0 (y + 1)) (Niveau l h (Grille ens))
  | y >= h = True
  | Map.lookup (C x y) ens == Nothing = False
  | otherwise = coords_associated (C (x + 1) y) (Niveau l h (Grille ens))



associated_inbound :: Niveau -> Bool
associated_inbound (Niveau l h (Grille ens)) = Map.foldlWithKey crd_inbound True ens where
  crd_inbound b (C x y) cse
    | b == False = False
    | x < 0 || x >= l || y < 0 || y >= h = False
    | otherwise = True

prop_associated :: Niveau-> Bool
prop_associated n = (coords_associated (C 0 0) n) && associated_inbound n

prop_NiveauCorrect :: Niveau-> Bool
prop_NiveauCorrect n = (prop_NiveauUniqueES n) && (prop_FermetureMetal n) && (prop_EntreeSurVide n) && (prop_SortieSurMetal n) && (prop_associated n)

encode_case :: Case -> String
encode_case M = "X"
encode_case V = " "
encode_case E = "E"
encode_case S = "S"
encode_case T = "0"
encode_case _ = " "

decode_case :: String -> Case
decode_case "X" = M
decode_case " " = V
decode_case "E" = E
decode_case "S" = S
decode_case "0" = T
decode_case _ = V


instance Show Case where
  show = encode_case

instance Read Case where
  readsPrec _ x = [(decode_case x, "")]



show_Niveau :: Niveau -> String
show_Niveau n = show_Niveau_rec (C 0 0) n "" where
  show_Niveau_rec (C x y) (Niveau l h ens) s
    | x >= l = show_Niveau_rec (C 0 (y + 1)) (Niveau l h ens) (s ++ "\n")
    | y >= h = s
    | otherwise = show_Niveau_rec (C (x + 1) y) (Niveau l h ens) (s ++ (show (getBox x y ens)))

instance Show Niveau where
  show n = show_Niveau n

build_Niveau :: (Int, Int, Grille) -> Niveau
build_Niveau (x, y, lemap) = Niveau y y lemap

parse_niveau :: (Int, Int, Grille) -> Char -> (Int, Int, Grille)
parse_niveau (x, y, lemap) c
  |c == '\n' = (0, y + 1, lemap)
  |otherwise = (x + 1, y, addBox x y (read [c]) lemap)




read_Niveau :: String -> Niveau
read_Niveau = build_Niveau . List.foldl' parse_niveau (0, 0, bempty)

instance Read Niveau where
  readsPrec _ x = [(read_Niveau x, "")]







dure :: Coord -> Niveau-> Bool
dure (C x y) (Niveau l h ens)
  |getBox x y ens == M || getBox x y ens == T = True
  |otherwise = False

passable :: Coord -> Niveau-> Bool
passable (C x y) (Niveau l h ens)
  |getBox x y ens  == V ||getBox x y ens  == E ||getBox x y ens  == S = True
  |otherwise = False

isLemming :: Coord -> Niveau-> Bool
isLemming (C x y) (Niveau l h ens)
  |getBox x y ens == L = True
  |otherwise = False

creusable :: Coord -> Grille -> Bool
creusable (C x y) ens
  |getBox x y ens == T = True
  |otherwise = False

posable :: Coord -> Grille -> Bool
posable (C x y) ens
  |getBox x y ens == V = True
  |otherwise = False

creuse :: Coord -> Grille -> Grille
creuse (C x y) ens
  |creusable (C x y) ens == True = addBox x y V ens
  |otherwise =  ens

--vérifie qu'on a bien creusé
post_creuse :: Coord -> Grille -> Bool
post_creuse (C x y) g = getBox x y g == V || getBox x y g == M


pose :: Coord -> Grille -> Grille
pose (C x y) ens
  |posable (C x y) ens == True = addBox x y T ens
  |otherwise = ens

--vérifie qu'on a bien posé
post_pose :: Coord -> Grille -> Bool
post_pose (C x y) g = getBox x y g /= V
