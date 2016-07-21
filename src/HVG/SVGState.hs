module HVG.SVGState where

import Text.Printf
import Data.List
import Control.Monad

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

type SVGDrawing = [Contour]
type ContourDrawing = [Path]
data PathDrawing = PathDrawing
  {-# UNPACK #-} !Pos -- begin pos
  [Segment] -- each seg without begin

newtype Contour = Contour [Path]
data Path = Path {-# UNPACK #-} !Pos [Segment]

data Segment
  = LineSeg {-# UNPACK #-} !Pos
  | BezierSeg {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos {-# UNPACK #-} !Pos
  deriving Show

segmentCommand :: Segment -> String
segmentCommand (LineSeg p) = printf "L %f" p
segmentCommand (BezierSeg c1 c2 p) = printf "C %f %f %f" c1 c2 p

pathCommand :: Path -> String
pathCommand path@(Path beginPos segs) = printf "M %f\n  " beginPos ++ pathCommandHalf path

pathCommandHalf :: Path -> String
pathCommandHalf (Path _ segs) = intercalate "\n  " $ map segmentCommand segs

contourCommand :: Contour -> String
contourCommand (Contour []) = ""
contourCommand (Contour (p:ps)) = pathCommand p ++ join (map (\hp -> ' ' : pathCommandHalf hp) ps) ++ " Z"

svgCommand :: SVGDrawing -> String
svgCommand [] = ""
svgCommand cs = "<path d=\"\n  " ++ intercalate "\n\n  " (map contourCommand cs) ++ "\n\" fill-rule=\"evenodd\" fill=\"green\" />"

instance TransAppable Segment where
  appTrans trans (LineSeg p) = LineSeg (appTrans trans p)
  appTrans trans (BezierSeg c1 c2 p) = BezierSeg (appTrans trans c1) (appTrans trans c2) (appTrans trans p)
  appInvTrans trans (LineSeg p) = LineSeg (appInvTrans trans p)
  appInvTrans trans (BezierSeg c1 c2 p) = BezierSeg (appInvTrans trans c1) (appInvTrans trans c2) (appInvTrans trans p)

contour :: Builder info SVGState ContourDrawing () -> Builder info SVGState SVGDrawing ()
contour (Builder buildContour) = do
  ctx <- getCtx
  case buildContour Nothing ctx (initBuilderStateWithDraw []) of
    BuilderPartDone _ _ bld _ -> addDraw [Contour (bldDraw bld)]
    _ -> return ()

pathMightReverse :: (Path -> Path) -> Pos -> Builder info SVGState PathDrawing () -> Builder info SVGState ContourDrawing ()
pathMightReverse rev beginPos (Builder buildPath) = local $ do
  appTr $ transition beginPos
  ctx <- getCtx
  tr <- getTr
  case buildPath Nothing ctx (initBuilderStateWithDraw (PathDrawing (appTrans tr (Pos 0 0)) [])) of
    BuilderPartDone _ _ bld _ -> addDraw [rev $ Path beginPos segs]
      where
        PathDrawing beginPos segs = bldDraw bld
    _ -> return ()

path :: Pos -> Builder info SVGState PathDrawing () -> Builder info SVGState ContourDrawing ()
path = pathMightReverse id

p :: Double -> Double -> Builder info SVGState PathDrawing () -> Builder info SVGState ContourDrawing ()
p x y = path (Pos x y)

rpath :: Pos -> Builder info SVGState PathDrawing () -> Builder info SVGState ContourDrawing ()
rpath = pathMightReverse reversePath

rp :: Double -> Double -> Builder info SVGState PathDrawing () -> Builder info SVGState ContourDrawing ()
rp x y = rpath (Pos x y)

reversePath :: Path -> Path
reversePath (Path beginPos segs) = go beginPos segs [] where
  go beginPos segs trailing = case segs of
    [] -> Path beginPos trailing
    (LineSeg endPos : segs) -> go endPos segs (LineSeg beginPos : trailing)
    (BezierSeg c1 c2 endPos : segs) -> go endPos segs (BezierSeg c2 c1 beginPos : trailing)

line :: Pos -> Builder info SVGState PathDrawing ()
line pos = do
  tr <- getTr
  PathDrawing beginPos segs <- getDraw

  putDraw $ PathDrawing beginPos (segs ++ [appTrans tr (LineSeg pos)])
  appTr $ transition pos

l :: Double -> Double -> Builder info SVGState PathDrawing ()
l x y = line (Pos x y)

bezier :: Pos -> Pos -> Pos -> Builder info SVGState PathDrawing ()
bezier c1 c2 pos = do
  tr <- getTr
  PathDrawing beginPos segs <- getDraw
  putDraw $ PathDrawing beginPos (segs ++ [appTrans tr (BezierSeg c1 c2 pos)])
  appTr $ transition pos

b :: Double -> Double -> Double -> Double -> Double -> Double -> Builder info SVGState PathDrawing ()
b x1 y1 x2 y2 x y = bezier (Pos x1 y1) (Pos x2 y2) (Pos x y)

arc :: Pos -> Angle -> Builder info SVGState PathDrawing ()
arc endPos angle = do
  tr <- getTr
  PathDrawing veryBeginPos segs <- getDraw

  let
    beginPos = Pos 0 0
    arcSegs = gen (abs angle) beginPos endPos [] where
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
      , let c1 = (revDis sShift `movePos` c0)
      , let c2 = (sShift `movePos` c0)
      = BezierSeg c1 c2 endPos : laterSegs

  putDraw $ PathDrawing veryBeginPos (segs ++ map (appTrans tr) arcSegs)
  appTr $ transition endPos

a :: Double -> Double -> Double -> Builder info SVGState PathDrawing ()
a x y deg = arc (Pos x y) (Deg deg)

-- class TransAppable a => IsSegment a where
--   segCmds :: a -> String
-- data Seg = forall seg. IsSegment seg => Seg seg
-- 
-- newtype SegPos = SegPosData {unSegPos :: Pos} deriving TransAppable
-- pattern SegPos x y = SegPosData (Pos x y)
-- 
-- instance PrintfArg SegPos where
--   formatArg (SegPos x y) fmt suffix = formatArg x fmt $ "," ++ formatArg y fmt suffix
-- 
-- data CloseSeg = CloseSeg
-- instance TransAppable CloseSeg where
--   appTrans _ _ = CloseSeg
--   appInvTrans _ _ = CloseSeg
-- instance IsSegment CloseSeg where
--   segCmds _ = "Z"
-- 
-- newtype BeginSeg = BeginSeg SegPos
-- pattern BeginSegPos x y = BeginSeg (SegPos x y)
-- instance TransAppable BeginSeg where
--   appTrans trans (BeginSeg pos) = BeginSeg $ appTrans trans pos
--   appInvTrans trans (BeginSeg pos) = BeginSeg $ appInvTrans trans pos
-- instance IsSegment BeginSeg where
--   segCmds (BeginSeg pos) = printf "M %f" pos
-- 
-- newtype LineSeg = LineSeg SegPos
-- pattern LineSegPos x y = LineSeg (SegPos x y)
-- instance TransAppable LineSeg where
--   appTrans trans (LineSeg pos) = LineSeg $ appTrans trans pos
--   appInvTrans trans (LineSeg pos) = LineSeg $ appInvTrans trans pos
-- instance IsSegment LineSeg where
--   segCmds (LineSeg pos) = printf "L %f" pos
-- 
-- data BezierSeg = BezierSeg
--   {-# UNPACK #-} !SegPos
--   {-# UNPACK #-} !SegPos
--   {-# UNPACK #-} !SegPos
-- pattern BezierSegPos x1 y1 x2 y2 x y = BezierSeg
--   (SegPos x1 y1)
--   (SegPos x2 y2)
--   (SegPos x y)
-- instance TransAppable BezierSeg where
--   appTrans trans (BezierSeg c1 c2 pos) =
--     BezierSeg (appTrans trans c1) (appTrans trans c2) (appTrans trans pos)
--   appInvTrans trans (BezierSeg c1 c2 pos) =
--     BezierSeg (appInvTrans trans c1) (appInvTrans trans c2) (appInvTrans trans pos)
-- instance IsSegment BezierSeg where
--   segCmds (BezierSeg c1 c2 pos) = printf "C %f %f %f" c1 c2 pos
-- 
-- data ArcSeg = ArcSeg [BezierSeg]
-- arcSeg :: SegPos -> SegPos -> Angle -> ArcSeg
-- arcSeg (SegPosData beginPos) (SegPosData endPos) angle = ArcSeg (gen (abs angle) beginPos endPos []) where
--   radius' = distance beginPos endPos / 2 / sin (unRad angle / 2)
--   radius = trace ("radius = " ++ show radius') radius'
-- 
--   stringRadiusTurn = if unRad angle > 0 then turnRightDis else turnLeftDis
-- 
--   --centerPos' = ((radius * cos (unRad angle / 2)) `scaleDis` stringRadiusTurn (unitDis $ dis endPos beginPos)) `movePos` midPos beginPos endPos
--   --centerPos' = ((radius * cos (unRad angle / 2)) `scaleDis` turnRightDis (unitDis $ dis endPos beginPos)) `movePos` midPos beginPos endPos
--   centerPos' = ((radius * cos (unRad angle / 2)) `scaleDis` turnLeftDis (unitDis $ dis endPos beginPos)) `movePos` midPos beginPos endPos
--   centerPos = trace ("center=" ++ show centerPos') centerPos'
-- 
--   -- <circle cx="19.3" cy="15.7" r="1" fill="red"/>
--   gen angle beginPos endPos laterSegs
--     | unDeg angle > 45
--     --, let sepPos' = stringRadiusTurn (scaleDis radius $ unitDis $ dis beginPos endPos) `movePos` centerPos
--     , let sepPos = turnLeftDis (scaleDis radius $ unitDis $ dis beginPos endPos) `movePos` centerPos
--     --, let sepPos = trace ("sepPos = " ++ show sepPos') sepPos'
--     , let halfAngle = angle / 2
--     = gen halfAngle beginPos sepPos (gen halfAngle sepPos endPos laterSegs)
--     | otherwise
--     , let stringDir' = unitDis (dis beginPos endPos)
--     , let stringDir = trace ("stringDir=" ++ show stringDir') stringDir'
--     --, let radiusDir = stringRadiusTurn stringDir
--     , let radiusDir' = turnLeftDis stringDir
--     , let radiusDir = trace ("radiusDir=" ++ show radiusDir') radiusDir'
--     , let rRefScale' = cos (unRad angle / 2)
--     , let sRefScale' = sin (unRad angle / 2)
--     , let rRefScale = trace ("rRefScale=" ++ show rRefScale') rRefScale'
--     , let sRefScale = trace ("sRefScale=" ++ show sRefScale') sRefScale'
--     , let rShiftScale' = (4 - rRefScale) / 3 * radius
--     , let sShiftScale' = (1 - rRefScale) * (3 - rRefScale) / (3 * sRefScale) * radius
--     , let rShiftScale = trace ("rShiftScale=" ++ show rShiftScale') rShiftScale'
--     , let sShiftScale = trace ("sShiftScale=" ++ show sShiftScale') sShiftScale'
--     , let rShift = rShiftScale `scaleDis` radiusDir
--     , let sShift = sShiftScale `scaleDis` stringDir
--     , let c0' = rShift `movePos` centerPos
--     , let c0 = trace ("c0=" ++ show c0') c0'
--     , let c1 = SegPosData (revDis sShift `movePos` c0)
--     , let c2 = SegPosData (sShift `movePos` c0)
--     = BezierSeg c1 c2 (SegPosData endPos) : laterSegs
-- instance TransAppable ArcSeg where
--   appTrans trans (ArcSeg bs) = ArcSeg $ map (appTrans trans) bs
--   appInvTrans trans (ArcSeg bs) = ArcSeg $ map (appInvTrans trans) bs
-- instance IsSegment ArcSeg where
--   segCmds (ArcSeg bs) = intercalate "\n" (map segCmds bs)
