module Ch9 where

import Data.Char
import Data.Time


myTail :: String -> String
myTail xs = dropWhile (== ' ') $ dropWhile (/= ' ') xs

myHead :: String -> String
myHead xs = takeWhile (/= ' ') xs

myWords :: [String] -> [String]
myWords xs = head xs : tail xs

xs :: String
xs = "Hello world Tom"

wordsR :: String -> [String]
wordsR [] = []
wordsR a@(x:xs)
  | x == ' ' = wordsR xs
  | otherwise = (myHead a) : wordsR (myTail a)

wordR' :: String -> [String]
wordR' [] = []
wordR' (' ':xs) = wordR' xs
wordR' xs = takeWhile(/= ' ') xs : wordR' (dropWhile (/= ' ') xs)

ptmSpace [] = []
ptmSpace (' ':xs) = ptmSpace xs
ptmSpace xs = xs

-- ex2
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines a@(x:xs)
  | x == '\n' = myLines xs
  | otherwise = takeWhile (/= '\n') a : myLines (dropWhile (/= '\n') a)

wordR'' :: String -> [String]
wordR'' [] = []
wordR'' xs = case az of
  [] -> []
  az -> az : wordR'' (dropWhile (/= ' ') wsz)
  where    wsz = (dropWhile (== ' ') xs)
           az = (takeWhile (/= ' ') wsz)

separate :: Char -> String -> [String]
separate _ [] = []
separate sep xs = case dropWhile (== sep) xs of
  [] -> []
  az -> w : separate sep rest
    where (w, rest) = break (== sep) az

myLines' :: String -> [String]
myLines' = separate '\n'

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []


capT :: String -> String
capT (x:xs) = toUpper x : capT xs
capT "" = ""

capHead :: String -> Char
capHead = toUpper . head

--

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f x == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem c [] = False
myElem c (x:xs) = if c == x then True else myElem c xs

myReverse :: [a] -> [a]
myReverse xs = go [] xs
  where go ys [] = ys
        go ys (x:xs) = go (x:ys) xs

myR' :: [a] -> [a]
myR' xs = go [] xs
  where go ys [] = ys
        go ys (x:xs) = go (x:ys) xs

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (xs) = squishMap (\x -> x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = go f x xs where
  go f b (x:xs)
    | f b x == GT = go f b xs
    | otherwise = go f x xs
  go f b [] = b


myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy _ [] = undefined
myMinBy _ (x:[]) = x
myMinBy f (x:xs) = go f x xs where
  go f b (x:xs)
    | f b x == LT = go f b xs
    | otherwise = go f x xs
  go f b [] = b

myMax :: (Ord a) => [a] -> a
myMax [] = undefined
myMax (x:xs) = go x xs where
  go b (x:xs)
    | b > x = go b xs
    | otherwise = go x xs
  go b [] = b


------
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
  where f (DbDate a) b = a : b
        f _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
  where f (DbNumber a) b = a : b
        f _ b = b


sumDb :: [DatabaseItem] -> Integer
sumDb = foldr f 0
  where f (DbNumber a) b = a + b
        f _ b = b


fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

facs = scanl (*) 1 [1..]

stops = "pbtdkg"
vowels = "aeiou"

svs stops vowels = [(s, v, s') | s <- stops, v <- vowels, s' <- stops, s == 'p']

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x))) (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (\a b -> if a == False then False else b) True

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\a b -> f a || b ) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = foldr (\x y -> if a == x then True else y) False

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\x y -> f x : y) [] xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' pr = foldr f [] where
  f a b
    | pr a = a : b
    | otherwise = b

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr (\a b -> f a ++  b) []

squishAgain' :: [[a]] -> [a]
squishAgain' = squishMap' id
