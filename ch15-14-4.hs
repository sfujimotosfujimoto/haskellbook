import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f ) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: ThreeAssoc)
