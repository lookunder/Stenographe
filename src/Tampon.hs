module Tampon where

import Data.Monoid
import qualified Data.Text as T

import Types

data Tampon = Tampon
  { avant :: [T.Text]
  , ligne :: T.Text
  , apres :: [T.Text]
  } deriving (Show)

type PosTampon = Position Tampon

tamponVide :: Tampon
tamponVide = Tampon [] T.empty []

aPartirDunText :: T.Text -> Tampon
aPartirDunText t =
  let (l:ls) = T.lines t
  in Tampon [] l ls

obtTexte :: Tampon -> T.Text
obtTexte = T.dropWhileEnd (=='\n') . T.unlines . obtListe

obtListe :: Tampon -> [T.Text]
obtListe (Tampon av l ap) = reverse av <> [l] <> ap

ligneCourante :: Tampon -> T.Text
ligneCourante (Tampon _ l _) = l

deplaceLigneSuivante :: PosTampon -> Tampon -> (PosTampon,Tampon)
deplaceLigneSuivante p t@(Tampon _ _ []) = (p,t)
deplaceLigneSuivante (Position x y) (Tampon av l (a:ap)) = (Position (min x (T.length a))(y+1), Tampon (l:av) a ap)

deplaceLignePrecedante :: PosTampon -> Tampon -> (PosTampon, Tampon)
deplaceLignePrecedante p t@(Tampon [] _ _) = (p,t)
deplaceLignePrecedante (Position x y) (Tampon (a:av) l ap) = (Position (min x (T.length a))(y-1), Tampon av a (l:ap))

deplacePositionPrecedante :: PosTampon -> Tampon -> (PosTampon, Tampon)
deplacePositionPrecedante p@(Position 0 0) t@(Tampon [] _ _)   = (p,t)
deplacePositionPrecedante   (Position 0 y)   (Tampon (a:av) l ap)  = (Position (T.length a) (y-1),Tampon av a (l:ap))
deplacePositionPrecedante   (Position x y) t                       = (Position (x-1) y, t)

deplacePositionSuivante :: PosTampon -> Tampon -> (PosTampon, Tampon)
deplacePositionSuivante (Position x y) t@(Tampon av l (a:ap)) | x == T.length l = (Position 0 (y+1),Tampon (l:av) a ap)
                                                              | otherwise       = (Position (x+1) y, t)
deplacePositionSuivante p@(Position x y) t@(Tampon _ l []  )  | x == T.length l = (p,t)
                                                              | otherwise       = (Position (x+1) y, t)

nouvelleLigne :: PosTampon -> Tampon -> (PosTampon, Tampon)
nouvelleLigne (Position x y) (Tampon av l ap) =
  let (debut, fin) = T.splitAt x l
  in (Position 0 (y+1), Tampon (debut:av) fin ap)

effacerAvant :: PosTampon -> Tampon -> (PosTampon, Tampon)
effacerAvant (Position 0 0) t                    = (Position 0 0 , t)
effacerAvant (Position 0 y) (Tampon (a:av) l ap) = (Position (T.length a) (y-1), Tampon av (a<>l) ap)
effacerAvant (Position x y) (Tampon av l ap)     =
  let debut = T.take (x-1) l
      fin   = T.drop x l
  in (Position (x-1) y, Tampon av (debut <> fin) ap)

effacerSuivant :: PosTampon -> Tampon -> (PosTampon, Tampon)

effacerSuivant (Position x y) (Tampon av l [])
  | T.length l == x = (Position x y, Tampon av l [])
  | otherwise = let debut = T.take (x-1) l
                    fin   = T.drop x l
                in (Position (x-1) y, Tampon av (debut <> fin) [])

effacerSuivant (Position x y) (Tampon av l (a:ap))
  | T.length l == x = (Position x y, Tampon av (l<>a) ap)
  | otherwise = let debut = T.take (x-1) l
                    fin   = T.drop x l
                in (Position (x-1) y, Tampon av (debut <> fin) (a:ap))

ajtChar :: PosTampon -> Char -> Tampon -> (PosTampon, Tampon)
ajtChar (Position x y) c (Tampon av l ap) =
  let (debut, fin) = T.splitAt x l
  in (Position (x+1) y, Tampon av (debut <> T.cons c fin) ap)

