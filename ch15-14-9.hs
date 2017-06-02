import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

newtype Combine a b =
  Combine { unCombine :: (a -> b)}

instance Semigroup b => Semigroup (Combine a b) where
  Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type CombineAssoc = Combine String Ordering -> Combine String Ordering -> Combine String Ordering -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: CombineAssoc)


