module Tree where

import Prelude

import Control.Applicative
import Control.Apply
import Data.Functor
import Data.Foldable
import Data.Monoid
import Data.Traversable

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch lft x rht) = Branch (f <$> lft) (f x) (f <$> rht)

instance foldableTree :: Foldable Tree where
  foldr f x (Branch l y r) = foldr f (f y (foldr f x r)) l
  foldr f x Leaf = x
  foldl f x (Branch l y r) = foldl f (f (foldl f x l) y) r
  foldl f x Leaf = x
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r
  foldMap f Leaf = mempty

