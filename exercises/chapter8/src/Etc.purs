module Etc where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.MonadPlus
import Control.MonadZero
import Data.Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

safeDivide' :: forall eff . Int -> Int -> Eff (err :: EXCEPTION | eff) Int
safeDivide' _ 0 = throwException (error "Divide by Zero")
safeDivide' a b = pure (a / b)

third :: forall a. Array a -> Maybe a
third a = do
  first <- tail a
  second <- tail first
  head second

sums :: Array Int -> Array Int
sums = sort <<< nub <<< foldM (\x y -> [x, x + y]) 0

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (a : as) = do
  keep <- f a
  xs <- filterM f as
  pure if keep then a : xs else xs

