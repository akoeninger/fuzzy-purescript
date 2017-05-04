module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Path (Path, ls)
import Data.Array
import Data.Array.Partial (head, tail)
import Data.Foldable (product)
import Data.Int (toNumber)
import Partial.Unsafe (unsafePartial)
import Math ((%))

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

evenInt :: Int -> Boolean
evenInt 0 = true
evenInt 1 = false
evenInt n = evenInt (n - 2)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

countEvens :: Array Int -> Int
countEvens arr =
  if null arr
    then 0
    else
      countEvens (unsafePartial tail arr) + if evenInt (unsafePartial head arr)
        then 1
        else 0

squares :: Array Number -> Array Number
squares arr = (\n -> n * n) <$> arr

removeNegatives :: Array Int -> Array Int
removeNegatives arr =  (\n -> n >= 0) <$?> arr

infix 8 filter as <$?>

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  i <- a
  j <- b
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

factorize :: Int -> Array(Int)
factorize n = do
  i <- 1 .. n
  guard $ toNumber(n) % toNumber(i) == toNumber(0)
  pure i

factorization :: Int -> Array (Array Int)
factorization n = [n] : do
  x <- factorize n
  guard $ x > 1 && x < n
  xs <- factorization (n / x)
  pure (x : xs)



