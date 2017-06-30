module Etc where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random
import Control.Monad.ST
import Control.MonadPlus
import Control.MonadZero
import Data.Array
import Data.Int (floor,toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Math

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

estimatePi :: Number -> Eff (random :: RANDOM) Number
estimatePi prescision = runST do
  ref <- newSTRef 0
  forE 0 (floor prescision) \_ -> do
    x <- random
    y <- random
    modifySTRef ref \o ->
      if (isInCircle $ distanceFromCenter x y) then  o + 1 else o
    pure unit
  pointsInsideCircle <- readSTRef ref
  pure ((toNumber (4 * pointsInsideCircle)) / prescision)
  where
    distanceFromCenter :: Number -> Number -> Number
    distanceFromCenter x y = ((x - 0.5) `pow` 2.0) + ((y - 0.5) `pow` 2.0)
    isInCircle :: Number -> Boolean
    isInCircle d = d < (0.5 `pow` 2.0)
