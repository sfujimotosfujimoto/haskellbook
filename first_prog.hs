module First_Prog where

import           Data.Char
import           Data.List
import qualified Data.Map  as Map
--import Geometry

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


messyMain :: IO ()
messyMain = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print ("Dear " ++ recipient ++ ", \n" ++
    "Thanks for buying " ++ title ++ "\nthanks, \n" ++
    author )



toPart recipient = "Dear " ++ recipient ++ ", \n"
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart author = "Thanks,\n" ++ author
createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

removeNonUppercase :: String -> String
removeNonUppercase st = [
  c | c <- st, c `elem` ['A'..'Z']
  ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Int -> Int
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' []    = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter ""         = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first lettr of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congrats!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)



badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan"     = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ "Fernando!"
greet name       = badGreeting ++ " " ++ name


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
   where (f:_) = firstname
         (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15


wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

wordNum' :: String -> [[String]]
wordNum' xs = group . sort $  words xs

wordNums' :: String -> [(String, Int)]
wordNums' = map (\ws -> (head ws, length ws)) . wordNum'

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
  [("betty", "555-2342")
  ,("bonnie", "234-7989")
  ,("patsy", "432-4563")
  ,("lucille", "543-9856")
  ,("bonnie", "9340-2834")
  ,("wendy", "432-2345")
  ,("patsy", "234-6437")
  ,("patsy", "948-3785")
  ,("wendy", "432-2345")
  ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
