module Data.Picture where

import Prelude

import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)

import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe(..))
import Global as Global
import Math as Math

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Point Picture

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
showShape (Clipped loc pic) = 
  "Clipped [location: " <> showPoint loc <> ", bounds: " <> show (showBounds (bounds pic)) <> "]"

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped _ pic) = bounds pic

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

infixl 4 union as \/

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

infixl 4 intersect as /\

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m | n > m     =  gcd (n - m) m
        | otherwise =  gcd n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _      = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

nChoosek :: Int -> Int -> Int
nChoosek 0 k = 1
nChoosek n 0 = 1
nChoosek n k | n == k = 1
nChoosek n k = nChoosek (n - 1) (k - 1) + nChoosek (n - 1) k

-- Chapter 5.6 Array Patterns

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _ = 0

-- Chapter 5.7 Record Patterns and Row Polymorphism
showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x

-- Chapter 5.8 Nested Patterns
type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

-- Chapter 5.9 Named Patterns
sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

-- Chapter 5.9 Exercises
sameCity :: Person -> Person -> Boolean
sameCity { address : { city: x } } { address: { city: y } } = x == y

-- Ex 2. Most general type of sameCity would be to any record with a city field
sameCity' :: { city :: String } -> { city :: String } -> Boolean
sameCity' { city: x } { city: y } = x == y 

livesInLA' :: { city :: String } -> Boolean
livesInLA' { city: "Los Angeles" } = true
livesInLA' _ = false

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton default _ = default

-- Chapter 5.10 Case Expressions
lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
           0 -> xs
           _ -> lzs (unsafePartial tail xs)

-- Chapter 5.13 Using ADTs
exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 0.0 }

    p2 :: Point
    p2 = Point { x: 100.0, y: 50.0 }

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

centerCircle :: Shape
centerCircle = Circle origin 10.0

scaleBy2 :: Shape -> Shape
scaleBy2 (Circle _ r) = Circle origin (r * 2.0)
scaleBy2 (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
scaleBy2 (Line (Point start) (Point end)) = Line scaledStart scaledEnd
  where
    diff = { x: end.x - start.x, y: end.y - start.y }
    scaledStart = Point { x: -diff.x, y: -diff.y }
    scaledEnd = Point { x: diff.x, y: diff.y }
scaleBy2 (Text _ text) = Text origin text
scaleBy2 (Clipped _ picture) = Clipped origin (map (scaleBy2) picture)

extractText :: Shape -> Maybe String
extractText (Text loc text) = Just text
extractText _ = Nothing

area :: Shape -> Number
area (Circle _ r) = Math.pi * r `Math.pow` 2.0
area (Rectangle c w h) = w * h
area _ = 0.0
