module Entity where

import HVG.Type
import HVG.SVG
import HVG.SVGState
import HVG.Geometry

import Control.Monad
import Data.Monoid

tr :: Double -> Double -> Builder info SVGState Draw () -> Builder info SVGState Draw ()
tr x y body = local $ do
  appTr (transitionPos x y)
  body

sc :: Double -> Double -> Builder info SVGState Draw () -> Builder info SVGState Draw ()
sc x y body = local $ do
  appTr (scale x y)
  body

rt :: Double -> Builder info SVGState Draw () -> Builder info SVGState Draw ()
rt deg body = local $ do
  appTr (rotateDeg deg)
  body

m :: Double -> Double -> Builder info SVGState Draw ()
m x y = do
  trans <- getTr
  let
    pos = appTrans trans (Pos x y)
    entity = BeginSeg (SegPosData pos)
  addDraw (Seg entity :)
  setCur pos

z :: Builder info SVGState Draw ()
z = do
  let
    entity = CloseSeg
  addDraw (Seg entity :)

l :: Double -> Double -> Builder info SVGState Draw ()
l x y = do
  trans <- getTr
  let
    endPos = appTrans trans (Pos x y)
    entity = LineSeg (SegPosData endPos)
  addDraw (Seg entity :)
  setCur endPos

b :: Double -> Double -> Double -> Double -> Double -> Double -> Builder info SVGState Draw ()
b x1 y1 x2 y2 x y = do
  trans <- getTr
  let
    endPos = appTrans trans $ Pos x y
    entity = appTrans trans $ BezierSegPos x1 y1 x2 y2 x y
  addDraw (Seg entity :)
  setCur endPos

-- a angle x y
--  angle > 0 : CCW
--  angle < 0 : CW
a :: Double -> Double -> Double -> Builder info SVGState Draw ()
a deg x y = do
  trans <- getTr
  beginPos <- getCur
  let
    beginPos' = appInvTrans trans beginPos
    endPos' = Pos x y
    endPos = appTrans trans endPos'
    entity = appTrans trans (arcSeg (SegPosData beginPos') (SegPosData endPos') (Deg deg))
  addDraw (Seg entity :)
  setCur endPos
