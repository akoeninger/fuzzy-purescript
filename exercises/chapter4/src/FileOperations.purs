module FileOperations where

import Prelude

import Data.Path (Path, ls)
import Data.Array (concatMap, (:))

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
