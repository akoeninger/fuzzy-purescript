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

