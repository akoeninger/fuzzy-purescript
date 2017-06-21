module Etc where

import Prelude
import Data.Array
import Data.Maybe

third :: forall a. Array a -> Maybe a
third a = do
  first <- tail a
  second <- tail first
  head second

