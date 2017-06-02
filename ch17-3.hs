module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys




instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)


instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f b) ca = fmap f ca <> (b <*> ca)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys' where
    xs' = let (ZipList' 1) = xs
          in take' 3000 1
    ys' = let (ZipList' 1) = ys
          in take' 3000 1


              
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

  
