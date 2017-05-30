module Ch7 where

functionC x y = if ( x > y ) then x else y

functionC' :: Ord a => a -> a -> a
functionC' x y =
  case ( x > y ) of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' :: Integral a => a -> a
ifEvenAdd2' n =
  case (even n) of
    True -> (n+2)
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135   = "too low"
  | x > 145   = "too high"
  | otherwise = "just right"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise = False

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = fst $ divMod x 10
        d = snd $ divMod xLast 10


hunsDigit:: Integral a => a -> a
hunsDigit x = d
  where xLast = fst $ divMod x 100
        d = snd $ divMod xLast 10

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y z =
  case z of
    True -> x
    False -> y


roundTrip :: (Show a , Read b) => a -> b
roundTrip = read . show

main = do
  print ((roundTrip 4) :: Int)
  print (id 4)


fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

sumR :: (Eq a, Num a) => a -> a
sumR max = go max 0 0 where
  go m carry i
    | i == (m + 1) = carry
    | otherwise = go m (carry + i)(i + 1)

sumR' :: (Eq a, Num a) => a -> a
sumR' n = go n 0 where
  go n acc
    | n == 0 = acc
    | otherwise = go (n - 1)(acc + n)

mulR :: (Integral a) => a -> a -> a
mulR n m = go n m 0 where
  go n m acc
    | m == 0 = acc
    | otherwise = go n (m - 1) ( acc + n)


fib :: (Eq a, Num a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

sumR'' :: (Eq a, Num a) => a -> a
sumR'' n = go n 0 where
n  go n acc
    | n == 0 = acc
    | otherwise = go (n-1) (acc + n)

sumRR :: (Eq a, Num a) => a -> a
sumRR 0 = 0
sumRR n = n + sumRR (n-1)

