module NiveauSpec where
import Test.Hspec
import Niveau




niveau1Test = do
  describe "Test du niveau 1" $ do
    it "teste le niveau 1" $ do
      niveau1 <- initNiveau "assets/niveau1.txt"
      prop_NiveauCorrect niveau1 `shouldBe` True

niveau2Test = do
  describe "Test du niveau 2" $ do
    it "teste le niveau 2" $ do
      niveau1 <- initNiveau "assets/niveau2.txt"
      prop_NiveauCorrect niveau1 `shouldBe` True


niveau3Test = do
  describe "Test du niveau 3" $ do
    it "teste le niveau 3" $ do
      niveau1 <- initNiveau "assets/niveau3.txt"
      prop_NiveauCorrect niveau1 `shouldBe` True

niveauSpec = do
  niveau1Test
