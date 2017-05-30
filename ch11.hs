{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ch11 where
import Data.Char


data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData


data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Manufacturer
  = Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChanceUnited
    deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 500)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

getSize :: Vehicle -> Integer
getSize (Plane _ (Size s)) = s

data Example = MakeExample Int deriving Show

----
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype Goats = Goats (Int, Int) deriving Show

1 = 0

instance TooMany Goats where
  tooMany (Goats (n, n')) = (n + n') > 42



----
data Person = Person { name :: String
                     , age :: Int} deriving (Eq, Show)


----
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)



unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f b = case f b of
               Nothing -> Leaf
               Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f m
          | m == n = Nothing
          | otherwise = Just (m+1, m, m+1)














--test
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

---- Convert Binary trees to lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : (preorder left ++ preorder right)
  

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (inorder left) ++ [a] ++  (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left ++ postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder


isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [][] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ [] = False
isSubsequenceOf a@(x:xs) b@(y:ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf a ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = undefined
capitalizeWords xs = map f $ words xs
  where f as@(s:st) = (as, toUpper s : st)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

--capitalizeParagraph :: String -> String
--capitalizeParagraph xs = map f $ words xs
--  where f as@(s:st) = toUpper s:st
