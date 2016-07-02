module HVG.SVGState where

import Text.Printf
import Data.List

import Debug.Trace

import HVG.Type
import HVG.Geometry

data SVGState = SVGState
  { svgTrans :: Trans
  , svgCursor :: Pos
  }
initSVGState :: SVGState
initSVGState = SVGState identityTrans (Pos 0 0)

setTr :: Trans -> Builder info SVGState draw ()
setTr trans = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{svgTrans = trans} bld ()

appTr :: Trans -> Builder info SVGState draw ()
appTr trans = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{svgTrans = appTrans (svgTrans ctx) trans} bld ()

getTr :: Builder info SVGState draw Trans
getTr = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (svgTrans ctx)

setCur :: Pos -> Builder info SVGState draw ()
setCur pos = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{svgCursor = pos} bld ()

getCur :: Builder info SVGState draw Pos
getCur = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (svgCursor ctx)

--------------------

class TransAppable a => IsSegment a where
  segCmds :: a -> String
data Seg = forall seg. IsSegment seg => Seg seg

newtype SegPos = SegPosData {unSegPos :: Pos} deriving TransAppable
pattern SegPos x y = SegPosData (Pos x y)

instance PrintfArg SegPos where
  formatArg (SegPos x y) fmt suffix = formatArg x fmt $ "," ++ formatArg y fmt suffix

data CloseSeg = CloseSeg
instance TransAppable CloseSeg where
  appTrans _ _ = CloseSeg
  appInvTrans _ _ = CloseSeg
instance IsSegment CloseSeg where
  segCmds _ = "Z"

newtype BeginSeg = BeginSeg SegPos
pattern BeginSegPos x y = BeginSeg (SegPos x y)
instance TransAppable BeginSeg where
  appTrans trans (BeginSeg pos) = BeginSeg $ appTrans trans pos
  appInvTrans trans (BeginSeg pos) = BeginSeg $ appInvTrans trans pos
instance IsSegment BeginSeg where
  segCmds (BeginSeg pos) = printf "M %f" pos

newtype LineSeg = LineSeg SegPos
pattern LineSegPos x y = LineSeg (SegPos x y)
instance TransAppable LineSeg where
  appTrans trans (LineSeg pos) = LineSeg $ appTrans trans pos
  appInvTrans trans (LineSeg pos) = LineSeg $ appInvTrans trans pos
instance IsSegment LineSeg where
  segCmds (LineSeg pos) = printf "L %f" pos

data BezierSeg = BezierSeg
  {-# UNPACK #-} !SegPos
  {-# UNPACK #-} !SegPos
  {-# UNPACK #-} !SegPos
pattern BezierSegPos x1 y1 x2 y2 x y = BezierSeg
  (SegPos x1 y1)
  (SegPos x2 y2)
  (SegPos x y)
instance TransAppable BezierSeg where
  appTrans trans (BezierSeg c1 c2 pos) =
    BezierSeg (appTrans trans c1) (appTrans trans c2) (appTrans trans pos)
  appInvTrans trans (BezierSeg c1 c2 pos) =
    BezierSeg (appInvTrans trans c1) (appInvTrans trans c2) (appInvTrans trans pos)
instance IsSegment BezierSeg where
  segCmds (BezierSeg c1 c2 pos) = printf "C %f %f %f" c1 c2 pos

data ArcSeg = ArcSeg [BezierSeg]
arcSeg :: SegPos -> SegPos -> Angle -> ArcSeg
arcSeg (SegPosData beginPos) (SegPosData endPos) angle = ArcSeg (gen (abs angle) beginPos endPos []) where
  radius' = distance beginPos endPos / 2 / sin (unRad angle / 2)
  radius = trace ("radius = " ++ show radius') radius'

  stringRadiusTurn = if unRad angle > 0 then turnRightDis else turnLeftDis

  --centerPos' = ((radius * cos (unRad angle / 2)) `scaleDis` stringRadiusTurn (unitDis $ dis endPos beginPos)) `movePos` midPos beginPos endPos
  --centerPos' = ((radius * cos (unRad angle / 2)) `scaleDis` turnRightDis (unitDis $ dis endPos beginPos)) `movePos` midPos beginPos endPos
  centerPos' = ((radius * cos (unRad angle / 2)) `scaleDis` turnLeftDis (unitDis $ dis endPos beginPos)) `movePos` midPos beginPos endPos
  centerPos = trace ("center=" ++ show centerPos') centerPos'

  -- <circle cx="19.3" cy="15.7" r="1" fill="red"/>
  gen angle beginPos endPos laterSegs
    | unDeg angle > 45
    --, let sepPos' = stringRadiusTurn (scaleDis radius $ unitDis $ dis beginPos endPos) `movePos` centerPos
    , let sepPos = turnLeftDis (scaleDis radius $ unitDis $ dis beginPos endPos) `movePos` centerPos
    --, let sepPos = trace ("sepPos = " ++ show sepPos') sepPos'
    , let halfAngle = angle / 2
    = gen halfAngle beginPos sepPos (gen halfAngle sepPos endPos laterSegs)
    | otherwise
    , let stringDir' = unitDis (dis beginPos endPos)
    , let stringDir = trace ("stringDir=" ++ show stringDir') stringDir'
    --, let radiusDir = stringRadiusTurn stringDir
    , let radiusDir' = turnLeftDis stringDir
    , let radiusDir = trace ("radiusDir=" ++ show radiusDir') radiusDir'
    , let rRefScale' = cos (unRad angle / 2)
    , let sRefScale' = sin (unRad angle / 2)
    , let rRefScale = trace ("rRefScale=" ++ show rRefScale') rRefScale'
    , let sRefScale = trace ("sRefScale=" ++ show sRefScale') sRefScale'
    , let rShiftScale' = (4 - rRefScale) / 3 * radius
    , let sShiftScale' = (1 - rRefScale) * (3 - rRefScale) / (3 * sRefScale) * radius
    , let rShiftScale = trace ("rShiftScale=" ++ show rShiftScale') rShiftScale'
    , let sShiftScale = trace ("sShiftScale=" ++ show sShiftScale') sShiftScale'
    , let rShift = rShiftScale `scaleDis` radiusDir
    , let sShift = sShiftScale `scaleDis` stringDir
    , let c0' = rShift `movePos` centerPos
    , let c0 = trace ("c0=" ++ show c0') c0'
    , let c1 = SegPosData (revDis sShift `movePos` c0)
    , let c2 = SegPosData (sShift `movePos` c0)
    = BezierSeg c1 c2 (SegPosData endPos) : laterSegs
instance TransAppable ArcSeg where
  appTrans trans (ArcSeg bs) = ArcSeg $ map (appTrans trans) bs
  appInvTrans trans (ArcSeg bs) = ArcSeg $ map (appInvTrans trans) bs
instance IsSegment ArcSeg where
  segCmds (ArcSeg bs) = intercalate "\n" (map segCmds bs)
