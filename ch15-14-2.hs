import Test.QuickCheck (Arbitrary, arbitrary, quickCheck)
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: IdentityAssoc)

