module CoordinatesSpec where

import Test.Hspec
import Test.QuickCheck

import Coordinates
import Lemmings
import Niveau


instance Arbitrary Coord where
  arbitrary = do
    x <- choose (0, 20)
    y <- choose (0, 20)
    return (C x y)

coordSpec0 = do
  describe "coord" $ do
    it "preserves the coordinate after a left right" $
      property $ \c -> prop_bougeCoordGaucheDroite c

coordSpec1 = do
  describe "coord" $ do
    it "is the same to move left or to move to the box left" $
      property $ \c -> prop_bougePlacableGauche (Lemming c Marcheur (Niveau 0 0 bempty) G 0)
