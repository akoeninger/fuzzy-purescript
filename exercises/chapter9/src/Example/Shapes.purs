module Example.Shapes where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random
import Data.Maybe (Maybe(..))
import Data.Traversable
import Data.Array
import Graphics.Canvas (Context2D, CANVAS, closePath, lineTo, moveTo, fillPath,
                        setFillStyle, arc, rect, getContext2D,
                        getCanvasElementById,
                        strokePath, setStrokeStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

type Point = { x :: Number, y :: Number }

renderPath
  :: forall eff
   . Context2D
  -> Array Point
  -> Eff (canvas :: CANVAS | eff) Context2D
renderPath ctx points = strokePath ctx $ do
  traverse (\point -> lineTo ctx point.x point.y) points
  closePath ctx
 
f :: Number -> Point
f n | n < 0.0 = f 0.0
    | n > 1.0 = f 1.0
    | otherwise = { x: 250.0 + (Math.cos n * 200.0), y: 250.0 + (Math.sin n * 100.0) }

main :: Eff (canvas :: CANVAS, random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx


  renderPath ctx [ { x: 100.0, y: 50.0 }, { x: 220.0, y: 45.0 }, { x: 325.0, y: 55.0} ]  
  renderPath ctx [{ x: 10.0, y: 20.0 }, { x: 30.0, y: 49.0 }, { x: 10.0, y: 60.0 }]

  points <- for (1 .. 500) \_ -> random
  setStrokeStyle "#EE00FF" ctx

  renderPath ctx $ f <$> points

  fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , w: 100.0
    , h: 100.0
    }

  setFillStyle "#00FF00" ctx

  strokePath ctx $ fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , r: 50.0
    , start: Math.pi * 9.0 / 8.0
    , end: Math.pi * 2.0
    }

  fillPath ctx $ do
    moveTo ctx 500.0 50.0
    lineTo ctx 460.0 130.0
    lineTo ctx 540.0 130.0
    arc ctx
     { x: 500.0
     , y: 90.0
     , r: 50.0
     , start: Math.pi
     , end: Math.pi * 2.0
     }
    closePath ctx

  setFillStyle "#FF0000" ctx

  setStrokeStyle "#EE11FF" ctx

  strokePath ctx $  fillPath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx
