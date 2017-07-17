module Example.Random where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, strokePath, fillPath, arc, setStrokeStyle,
                        setFillStyle, getContext2D, getCanvasElementById)
import Math as Math
import Partial.Unsafe (unsafePartial)

strokeAndFillPath :: forall eff a
  . String
  -> String
  -> Context2D
  -> Eff (canvas :: CANVAS | eff) a
  -> Eff (canvas :: CANVAS | eff) a
strokeAndFillPath fillStyle strokeStyle ctx canEff = do
  setFillStyle fillStyle ctx
  setStrokeStyle strokeStyle ctx
  canEff 
  

main :: Eff (canvas :: CANVAS, random :: RANDOM) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  
  strokeAndFillPath "#FF0000" "#0000F0" ctx do
    for_ (1 .. 100) \_ -> do
      x <- random
      y <- random
      r <- random

      let path = arc ctx
           { x     : x * 600.0
           , y     : y * 600.0
           , r     : r * 50.0
           , start : 0.0
           , end   : Math.pi * 2.0
           }

      fillPath ctx path
      strokePath ctx path
