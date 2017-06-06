module FileOperations where

import Prelude (bind, map, not, pure, ($), (&&), (*), (+), (-), (/), (<), (<$>),
  (<<<), (<>), (==), (>), (>=))


import Control.MonadZero (guard)
import Data.Path (Path, filename, ls, isDirectory, size, root)
import Data.Array (concatMap, head, filter, length, null, (..), (:))
import Data.Array.Partial as DAPartial
import Partial.Unsafe (unsafePartial)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Math ((%))


fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

factTR :: Int -> Int -> Int
factTR 0 acc = acc
factTR n acc = factTR (n - 1) (acc * n)

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
      countEvens (unsafePartial DAPartial.tail arr) + if evenInt (unsafePartial DAPartial.head arr)
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

reverse :: forall a. Array a -> Array a
reverse = reverse' []
  where
    reverse' acc [] = acc
    reverse' acc xs = reverse' (unsafePartial DAPartial.head xs : acc)
                               (unsafePartial DAPartial.tail xs)
allTrue :: Array(Boolean) -> Boolean
allTrue bs = foldl (&&) true bs

isFalseSingletonArray :: Array(Boolean) -> Boolean
isFalseSingletonArray bs = foldl (==) false bs

count :: forall a. (a -> Boolean) -> Array a -> Int
count p = count' p 0
  where
    count' _ acc [] = acc
    count' pred acc xs = if pred (unsafePartial DAPartial.head xs)
                        then count' pred (acc + 1) (unsafePartial DAPartial.tail xs)
                        else count' pred acc (unsafePartial DAPartial.tail xs)

reverseFL :: forall a. Array a -> Array a
reverseFL = foldl (\xs x -> [x] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles file = 
  if isDirectory file
    then do
      child <- ls file
      onlyFiles child
    else [file]

onlyFiles' :: Path -> Array Path
onlyFiles' = filter(isFile) <<< allFiles where
  isFile = not <<< isDirectory

sizes :: Path -> Array (Maybe Int)
sizes p = map (\f -> size f) (onlyFiles p)

compareFile :: (Maybe Int -> Maybe Int -> Boolean) -> Path -> Path -> Maybe Path
compareFile op x y = if op (size x) (size y) then Just x else Just y 
 
largestFile :: Path -> Maybe Path
largestFile = foldl largeFile Nothing <<< onlyFiles' where
  largeFile (Just x) y = compareFile (>) x y
  largeFile Nothing y = Just y

smallestFile :: Path -> Maybe Path
smallestFile = foldl smallFile Nothing <<< onlyFiles' where
  smallFile (Just x) y = compareFile (<) x y
  smallFile Nothing y = Just y

whereIs :: String -> Maybe Path
whereIs n = head $ do
      path <- allFiles root
      child <- ls path
      guard $ filename child == n
      pure path

