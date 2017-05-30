module TopOrLocal where

addEx x = x ++ "!"

drop4 x = (take 1 . drop 4) x

dropAlot x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! (x - 1)

rvrs :: String
rvrs = three ++ " " ++ two ++ " " ++ one
  where x = "Curry is awesome"
        one = take 5 x
        two = take 2 (drop 6 x)
        three = drop 9 x
