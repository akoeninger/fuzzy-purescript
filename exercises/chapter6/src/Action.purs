module Action where

import Prelude
import Data.Monoid
import Data.Monoid.Additive (Additive(..))
import Data.Array

class Monoid m <= Action m a where
  act :: m -> a -> a

instance repeatAction :: Action (Additive Int) String where
  act (Additive n) s = repeat n s where
    repeat 0 _ = ""
    repeat m s = s <> repeat (m - 1) s

instance arrayAction :: Action m a => Action m (Array a) where
  act x xs = map (act x) xs

newtype Self m = Self m

instance selfAction :: Action m a => Action m (Self m) where
  act _ (Self m) = Self(m <> m)

