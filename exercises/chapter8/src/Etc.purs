module Etc where

import Prelude
import Data.Array
import Data.Maybe (Maybe)

third :: forall a. Array a -> Maybe a
third a = do
  first <- tail a
  second <- tail first
  head second

sums :: Array Int -> Array Int
sums = sort <<< nub <<< foldM (\x y -> [x, x + y]) 0

