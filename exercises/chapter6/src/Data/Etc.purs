module Data.Etc where

import Prelude
import Data.Foldable

-- 6.4 Show and Eq instances for Complex
newtype Complex = Complex { real :: Number, imaginary :: Number }

instance showComplex :: Show Complex where
  show (Complex c) = 
    "Complex [real: " <> show c.real <> ", imaginary: " <> show c.imaginary <> "]"

instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary
-- 6.5-7 Type Annotations

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys


instance semigroupNonEmpty :: Semigroup a => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f z (NonEmpty x xs) = foldr f z ([x] <> xs)
  foldl f z (NonEmpty x xs) = foldl f z ([x] <> xs)
  foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite x) (Finite y) = eq x y

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f z (OneMore x ys) = f x (foldr f z ys)
  foldl f z (OneMore x ys) = f (foldl f z ys) x
  foldMap f (OneMore x ys) = (f x) <> (foldMap f ys)

