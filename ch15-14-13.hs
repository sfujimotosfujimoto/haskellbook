import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>))

data Validation a b =
    Failure
  | Success b
  deriving (Eq, Show)

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Success a)) <> (AccumulateBoth (Failure b)) =
    AccumulateBoth (Success (a <> b))
  (AccumulateBoth (Failure a)) <> (AccumulateBoth (Failure b)) =
    AccumulateBoth (Failure (a <> b))
  
