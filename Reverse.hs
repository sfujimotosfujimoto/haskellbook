module Reverse where

rvrs :: String -> String
rvrs x = three ++ " " ++ two ++ " " ++ one
  where
        one = take 5 x
        two = take 2 $ drop 6 x
        three = drop 9 x

main :: IO ()
main = print ()
