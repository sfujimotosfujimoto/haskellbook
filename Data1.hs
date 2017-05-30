module Data1 where

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False


data Date =
  Date DayOfWeek Int

  
instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

f :: Int -> Bool
f 1 = True
f _ = False



