module Coordinates where

data Coord = C Int Int
  deriving (Show, Eq)

data Deplacement = N | G | D | H | B | GH | GB | DH | DB
  deriving (Eq, Show)

bougeCoord :: Deplacement  -> Coord -> Coord
bougeCoord dep (C x y)
 | dep == G = C (x - 1) y
 | dep == D = C(x + 1) y
 | dep == H = C x (y - 1)
 | dep == B = C x (y + 1)
 | dep == GH = C (x - 1) (y - 1)
 | dep == GB = C(x - 1) (y + 1)
 | dep == DH = C(x + 1) (y - 1)
 | dep == DB = C(x + 1) (y + 1)
 | otherwise = C x y

instance Ord Coord where
  (<=) (C x1 y1) (C x2 y2) = (y1 == y2 && x1 <= x2) || y1 < y2


distance :: Coord -> Coord -> Int
distance (C x y) (C a b) = (abs (x - a) ) + abs (y - b)

prop_bougeCoordGaucheDroite ::Coord -> Bool
prop_bougeCoordGaucheDroite c = c == bougeCoord D  (bougeCoord G c)

class Placable p where
  coordP :: p -> Coord
  bougeP :: p -> Deplacement -> p
  deplaceP :: p -> Coord -> p

prop_bougePlacableGauche :: Placable a => a -> Bool
prop_bougePlacableGauche p = coordP (bougeP p G) == coordP (deplaceP p (bougeCoord G (coordP p)))
