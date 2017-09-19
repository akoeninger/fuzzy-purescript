module Example.RandomCircle where 

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import Data.Foldable (for_)
import Data.Array ((..))
import Data.Int
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, strokePath, fillPath, arc, setStrokeStyle,
                        setFillStyle, getContext2D, getCanvasElementById, withContext)
import Math as Math
import Partial.Unsafe (unsafePartial)
import DOM (DOM)

rgbString :: Number -> Number -> Number -> String
rgbString r g b = "RGB(" <> show (floor r) <> ", " <> show (floor g) <> ", " <> show (floor b) <> ")"

render :: forall eff. Context2D -> Eff (canvas :: CANVAS
                                       , random :: RANDOM
                                       , console :: CONSOLE 
                                       | eff
                                       ) Context2D
render ctx = do
  x <- random
  y <- random
  r <- random
  let color = rgbString (x * 255.0) (y * 255.0) (r * 255.0)
  log color  
  setFillStyle color ctx
  setStrokeStyle color ctx

  withContext ctx do  
    let path = arc ctx
         { x : x * 600.0
         , y : y * 600.0
         , r : r * 25.0
         , start : 0.0
         , end : Math.pi * 2.0
         }
    fillPath ctx path
    strokePath ctx path

main :: Eff ( canvas :: CANVAS
            , random :: RANDOM
            , dom :: DOM
            , console :: CONSOLE
            ) Unit
main = void $ unsafePartial do   
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  node <- querySelector "#canvas"
  for_ node $ addEventListener "click" $ void do
    render ctx
