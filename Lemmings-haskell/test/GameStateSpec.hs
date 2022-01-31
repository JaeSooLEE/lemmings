module GameStateSpec where
import Test.QuickCheck
import Test.Hspec
import Niveau
import Moteur
import Lemmings
import Coordinates



niveauTest = read "XXXXXXX\nXXXEXXX\nX   0 X\nX     X\nX   S X\nXXXXXXX\nXXXXXXX\n"
nT = (lemmingId $ gameState_fullLem niveauTest)

--teste si le gamesstate reste cohérent à chaque tour sans intervention extérieure
prop_evolution_coherente :: GameState -> Bool
prop_evolution_coherente gs = aux 1 gs

aux :: Int -> GameState -> Bool
aux i gstate
  |i == 500 = True
  |rem i 50 == 0 = post_ajout_lemming gstate && gameState_inv gstate && aux (i + 1) (moteurJeu gstate (i + 1))
  |otherwise = gameState_inv gstate && aux (i + 1) (moteurJeu gstate (i + 1))

gameState_fullLem_aux :: GameState -> Int -> GameState
gameState_fullLem_aux gs i
  |i == 500 = gs
  |otherwise = gameState_fullLem_aux (moteurJeu gs (i + 1)) (i + 1)

gameState_fullLem :: Niveau -> GameState
gameState_fullLem n=   let gs = initGameState n in
  gameState_fullLem_aux gs 0

genId :: Gen Int
genId = do
  idt <- choose (0, 9)
  return idt

--prop_suppr :: Property
--prop_suppr = forAll genId $ post_suppr_lemming (enleverLemming)

deroulementTest = do
  describe "Test GameState" $ do
    it "test déroulement" $ do
      niveau1 <- initNiveau "assets/niveau1.txt"
      prop_evolution_coherente (initGameState niveau1) `shouldBe` True

supprTest = do
  describe "Test GameState" $ do
    it "test suppression" $ forAll genId $ \x -> (pre_manip_lemming (gameState_fullLem niveauTest) x) ==> post_suppr_lemming (enleverLemming (gameState_fullLem niveauTest) x) x




gameStateSpec = do
  deroulementTest
  supprTest
