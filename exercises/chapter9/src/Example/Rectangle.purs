module Example.Rectangle where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById, strokePath)
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  strokePath ctx $ fillPath ctx $ do 
    rect ctx
      { x: 250.0
      , y: 250.0
      , w: 100.0
      , h: 100.0
      }
    rect ctx
      { x: 350.0
      , y: 250.0
      , w: 100.0
      , h: 100.0
      }
