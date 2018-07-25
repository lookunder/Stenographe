{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Tampon
import Types

t = aPartirDunText "Ligne 1\nLigne 2\nLigne 3"

t1 = Tampon ["Ligne 2","Ligne 1"] "Ligne 3" ["Ligne 4","Ligne 5"]

main :: IO ()
main = hspec $
  describe "Prelude.head" $ do
    it "Retourne un Tampon vide." $ (T.length . obtTexte) tamponVide `shouldBe` 0

    it "Retourne un Tampon vide." $ (length . obtListe) t `shouldBe` 3

    it "Retourne un Tampon vide." $ (T.length . obtTexte) t `shouldBe` 23

    it "Contening 5 elements" $ (length . obtListe) t1 `shouldBe` 5

    it "Contening 5 elements" $ (T.length . obtTexte) t1 `shouldBe` 39

    it "ligne courante" $ ligneCourante t1 `shouldBe` "Ligne 3"

    it "Ajouter char" $ do
      let (pos,tampon) = ajtChar (Position 0 0) '1' t1
      ligneCourante tampon `shouldBe`  "1Ligne 3"
      pos `shouldBe` Position 1 0

    it "Ajouter char" $ do
      let (pos,tampon) = ajtChar (Position 7 0) '1' t1
      ligneCourante tampon `shouldBe` "Ligne 31"
      pos `shouldBe` Position 8 0

    it "Ajouter char" $ do
      let (pos,tampon) = ajtChar (Position 4 0) '1' t1
      ligneCourante tampon `shouldBe` "Lign1e 3"
      pos `shouldBe` Position 5 0

    it "Nouvelle Ligne" $ do
      let (pos,tampon) = nouvelleLigne (Position 4 0) t1
      ligneCourante tampon `shouldBe` "e 3"
      pos `shouldBe` Position 0 1
      let (pos2,tampon2) = deplaceLignePrecedante pos tampon
      ligneCourante tampon2 `shouldBe` "Lign"

    it "Ligne Precedante" $ do
      let (pos,tampon) = deplaceLignePrecedante (Position 4 0) t1
      ligneCourante tampon `shouldBe` "Ligne 2"
      --pos `shouldBe` (Position 0 0)
