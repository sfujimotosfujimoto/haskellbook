module Ch12 where

import Data.List



r :: a -> f a
r a = undefined

notThe :: String -> Maybe String
notThe xs
  | xs == "the" = Nothing
  | otherwise = Just xs

athe :: Maybe String -> String
athe Nothing = "a"
athe (Just x) = x


replaceThe :: String -> String
replaceThe str = concat $ intersperse " " $ map athe $ fmap notThe $ words str
  --where athe Nothing = "a"
  --      athe (Just x) = x

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = f 0 $ words s

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

f :: Integer -> [String] -> Integer
f n (x:y:ys)
  | x == "the" && isVowel (head y) = f (n + 1) (y:ys)
  | otherwise = f n ys
f n _ = n


wordVowels :: String -> String
wordVowels = filter isVowel

countVowels :: String -> Int
countVowels = length . wordVowels

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = if lv < lc then Just (Word' w) else Nothing
  where lv = length $ filter ((flip elem) vowels) w
        lc = length w - lv

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (tn i)
  where tn i
          | i == 0 = Zero
          | otherwise = Succ (tn (i -1))

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybee a id m

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = (Just x)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:ms) = catMaybes ms
catMaybes (Just a:ms) = a : catMaybes ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = foldr f (Just []) xs
  where f _ Nothing = Nothing
        f Nothing _ = Nothing
        f (Just a) (Just b) = Just (a:b)

lefts' :: [Either a b] -> [a]
lefts' as = foldr f [] as
  where f (Left a) xs = a : xs
        f (Right b) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Left a) xs = xs
        f (Right b) xs = b : xs
  
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = foldr f ([], []) xs
  where f (Left a) (xs, ys) = (a : xs, ys)
        f (Right b) (xs, ys) = (xs, b : ys)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left a) = fl a
either' _ fr (Right b) = fr b

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing -> []
  Just (x, y) -> x : myUnfoldr f y

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)

  
niceSum :: Num a => [a] -> a
niceSum = foldl (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)
