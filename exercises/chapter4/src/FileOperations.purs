module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, (:), (..), filter, null)
import Data.Array.Partial (head, tail)
import Data.Foldable (product)
import Partial.Unsafe (unsafePartial)

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
factors n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

