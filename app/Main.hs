{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Control.Monad
import Data.Char (chr, ord)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GetChar as GC

import System.Console.ANSI
import System.IO
import Tampon
import System.Console.Terminal.Size
import Types
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)

delete = chr 127
escape = chr 27
espace = chr 32
tilde = chr 126

data Mode = Edition | Commande | Quitter | Sauvegarde

data Cadre = Cadre {posCodre::PosTampon, dim::Dimension}

type PosCurseur = Position Cadre

aAfficher :: Cadre -> Tampon -> [T.Text]
aAfficher (Cadre (Position pX pY) (Dimension dX dY)) t =
  let contenu = obtListe t
      lignes = (take dY . drop pY) contenu
      colonnes = fmap (T.take dX . T.drop pX) lignes
  in colonnes

afficher :: Tampon -> Cadre -> IO ()
afficher t c = do
  let contenu = aAfficher c t
  clearScreen
  setCursorPosition 0 0
  mapM_ TIO.putStrLn contenu

afficherCurseur :: PosCurseur -> IO ()
afficherCurseur (Position pX pY) = setCursorPosition pY pX

posCurseur :: PosTampon -> Cadre -> PosCurseur
posCurseur (Position pTx pTy) (Cadre (Position pCx pCy) _) = Position (pTx-pCx) (pTy-pCy)

ajusterCadre :: PosTampon -> Cadre -> Cadre
ajusterCadre (Position pTx pTy) (Cadre (Position pCx pCy) d@(Dimension deltaX deltaY)) =
  let pCx' = nouvellePos pTx pCx (deltaX-1)
      pCy' = nouvellePos pTy pCy (deltaY-1)
  in Cadre (Position pCx' pCy') d
  where
    nouvellePos pT pC delta | pT < pC = pT
                            | pT > pC + delta = pT - delta
                            | otherwise = pC

data Etat = Etat
  { posT::PosTampon
  , tampon::Tampon
  , mode::Mode
  , cadre::Cadre
  , filepath::FilePath
  }

miseAJour :: Etat -> Char -> Etat
miseAJour  etat c =
  case mode etat of
    Commande ->
      case c of
        'i' -> etat { mode=Edition }
        'q' -> etat { mode=Quitter }
        's' -> etat { mode=Sauvegarde } --sauvegarde (tampon etat') (filepath etat)
        'A' ->  let (p',t') = deplaceLignePrecedante (posT etat) (tampon etat)
                in etat { posT=p', tampon=t', mode=Edition }

        'B' ->  let (p',t') = deplaceLigneSuivante (posT etat) (tampon etat)
                in etat { posT=p', tampon=t', mode=Edition }

        'C' ->  let (p',t') = deplacePositionSuivante (posT etat) (tampon etat)
                in etat { posT=p', tampon=t', mode=Edition }

        'D' ->  let (p',t') = deplacePositionPrecedante (posT etat) (tampon etat)
                in etat { posT=p', tampon=t', mode=Edition }

        _  -> etat { mode=Commande }

    Edition
      | c <= tilde && c>= espace ->
                   let (p',t') = ajtChar (posT etat) c (tampon etat)
                   in etat { posT=p', tampon=t' }

      | c == '\n' ->
                  let (p',t') = nouvelleLigne (posT etat) (tampon etat)
                  in etat { posT=p', tampon=t' }

      | c == '\DEL' ->
                  let (p',t') = effacerAvant (posT etat) (tampon etat)
                  in etat { posT=p', tampon=t' }

      | c == delete ->
                  let (p',t') = effacerSuivant (posT etat) (tampon etat)
                  in etat { posT=p', tampon=t' }

      | c == escape -> etat { mode=Commande }

      | otherwise -> etat

process :: Etat -> IO ()
process etat = do
  mSize <- size

  let cadre' = case mSize of
             Just (Window h w) -> ajusterCadre (posT etat) ((cadre etat) { dim = Dimension (w-1) (h-1)})
             Nothing           -> ajusterCadre (posT etat) (cadre etat)

      positionDansCadre = posCurseur (posT etat) cadre'
      etat' = etat { cadre=cadre' }

  afficher (tampon etat') cadre'
  afficherCurseur positionDansCadre
  c <- GC.getChar
  let etat'' = miseAJour etat c
  case mode etat'' of
    Quitter    -> return ()
    Sauvegarde -> sauvegarde (tampon etat'') (filepath etat'') >> process etat''
    _          -> process etat''

ouvrir :: FilePath -> IO Tampon
ouvrir filepath = aPartirDunText <$> TIO.readFile filepath

sauvegarde :: Tampon -> FilePath -> IO ()
sauvegarde tampon filepath = TIO.writeFile filepath (obtTexte tampon)

--data Options = Options {
--        fichier :: FilePath
--      } deriving Show

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  setTitle "Sténographe"
  showCursor

--  tamponInit <- case mFichier of
--    Just f -> ouvrir
--    Nothing -> return tamponVide
  process $ Etat (Position 0 0) tamponVide Edition (Cadre (Position 0 0) (Dimension 80 23)) ""
