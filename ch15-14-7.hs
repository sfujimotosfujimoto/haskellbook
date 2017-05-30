import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj a1) = BoolDisj (a || a1)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    elements [(BoolDisj a), (BoolDisj a)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
