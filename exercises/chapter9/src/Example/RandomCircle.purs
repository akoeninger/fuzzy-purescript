module Example.RandomCircle where

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
import Dom (DOM)

main :: Eff ( canvas :: CANVAS
            , random :: RANDOM
            , dom :: DOM
            ) Unit
main = void $ unsafePartial do   
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas


