import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj a1) = BoolConj (a && a1)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    elements [(BoolConj a), (BoolConj a)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)

