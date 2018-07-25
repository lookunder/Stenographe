module Types where


type Largeur = Int

type Hauteur = Int

data Position a = Position !Int !Int deriving (Eq, Show)

data Dimension = Dimension !Largeur !Hauteur deriving (Eq, Show)

