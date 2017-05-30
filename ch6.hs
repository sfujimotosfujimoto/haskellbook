module Ch6 where
import Data.List



type Subject = String
type Verb = String
type Object = String

data Sentence  =
  Sentence Subject Verb Object
  deriving (Eq, Show)

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => a -> a
signifier xs = head (mySort xs)

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives
