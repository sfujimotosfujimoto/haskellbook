zzzmodule Cipher where

import Data.Char


aToN c = case isUpper c of
  True -> ord c - 65
  False -> ord c - 97


  
docyp :: Int -> Char -> Char
docyp n c
  | isUpper c = chr $ (mod (aToN c + n) 26) + 65
  | otherwise = chr $ (mod (aToN c + n) 26) + 97


caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) n
  | isAlpha x = docyp n x : caesar xs n
  | otherwise = x : caesar xs n

 
vigenere :: String -> String -> String
vigenere xs ys = vigenere' xs (cycle ys)

vigenere' [] _ = ""
vigenere' (' ':xs) cyp        = ' ' : vigenere' xs cyp
vigenere' (x:xs)   cyp@(y:ys) = docyp x y : vigenere' xs ys
  where base      = ord 'A'
        r         = 26
        dist c    = ord c - base
        docyp x y = chr $ (dist x + dist y) `mod` r + base

main = print $ vigenere "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
