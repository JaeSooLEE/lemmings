import CoordinatesSpec
import NiveauSpec
import GameStateSpec
import Test.Hspec
import Moteur
import Niveau



main :: IO ()
main = hspec $ do
  coordSpec0
  coordSpec1
  niveauSpec
  gameStateSpec
