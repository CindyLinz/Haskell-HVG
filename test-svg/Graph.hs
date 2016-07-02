module Graph where

import Data.Traversable
import Control.Monad

import HVG.Type
import HVG.SVG
import HVG.SVGState

import Entity

graph :: Builder () SVGState Draw ()
graph = do
  rt 15 $ do
    sc 10 20 $ do
    --tr 10 10 $ do
        m 10 10
        l 20 10
        a (180) 25 15
        a (180) 20 10
  --return ()
