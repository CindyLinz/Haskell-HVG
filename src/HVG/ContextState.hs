module HVG.ContextState where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import HVG.Type

type Link = [LinkPoint]

class StringValue a where
  strValue :: a -> String

data TextAlign
  = TextStart
  | TextEnd
  | TextLeft
  | TextRight
  | TextCenter
instance StringValue TextAlign where
  strValue = \case
    TextStart -> "start"
    TextEnd -> "end"
    TextLeft -> "left"
    TextRight -> "right"
    TextCenter -> "center"

data TextBaseline
  = TextTop
  | TextHanging
  | TextMiddle
  | TextAlphabetic
  | TextIdeographic
  | TextBottom
instance StringValue TextBaseline where
  strValue = \case
    TextTop -> "top"
    TextHanging -> "hanging"
    TextMiddle -> "middle"
    TextAlphabetic -> "alphabetic"
    TextIdeographic -> "ideographic"
    TextBottom -> "bottom"

data LineCap
  = LineCapButt
  | LineCapRound
  | LineCapSquare
instance StringValue LineCap where
  strValue = \case
    LineCapButt -> "butt"
    LineCapRound -> "round"
    LineCapSquare -> "square"

data LineJoin
  = LineJoinBevel
  | LineJoinRound
  | LineJoinMiter
instance StringValue LineJoin where
  strValue = \case
    LineJoinBevel -> "bevel"
    LineJoinRound -> "round"
    LineJoinMiter -> "miter"

data CompositeOp
  = CompositeSourceAtop
  | CompositeSourceIn
  | CompositeSourceOut
  | CompositeSourceOver
  | CompositeDestinationAtop
  | CompositeDestinationIn
  | CompositeDestinationOut
  | CompositeDestinationOver
  | CompositeLighter
  | CompositeCopy
  | CompositeXor
  | CompositeVendorSpec String String
instance StringValue CompositeOp where
  strValue = \case
    CompositeSourceAtop -> "source-atop"
    CompositeSourceIn -> "source-in"
    CompositeSourceOut -> "source-out"
    CompositeSourceOver -> "source-over"
    CompositeDestinationAtop -> "destination-atop"
    CompositeDestinationIn -> "destination-in"
    CompositeDestinationOut -> "destination-out"
    CompositeDestinationOver -> "destination-over"
    CompositeLighter -> "lighter"
    CompositeCopy -> "copy"
    CompositeXor -> "xor"
    CompositeVendorSpec vendorName operationName -> vendorName ++ "-" ++ operationName

data RotateDirection
  = CW
  | CCW

data ContextState = ContextState
  { ctxTransform :: Matrix
  , ctxSize :: Size

  , ctxFill :: Maybe String
  , ctxStroke :: Maybe String

  , ctxLineWidth :: Double
  , ctxLineCap :: LineCap
  , ctxLineJoin :: LineJoin
  , ctxMiterLimit :: Double
  , ctxLineDash :: [Double]
  , ctxLineDashOffset :: Double

  , ctxTextAlign :: TextAlign
  , ctxTextBaseline :: TextBaseline
  , ctxFont :: String
  }
initContextState :: Size -> ContextState
initContextState size = ContextState
  { ctxTransform = identityMatrix
  , ctxSize = size

  , ctxFill = Nothing
  , ctxStroke = Nothing

  , ctxLineWidth = 1
  , ctxLineCap = LineCapButt
  , ctxLineJoin = LineJoinMiter
  , ctxMiterLimit = 10
  , ctxLineDash = []
  , ctxLineDashOffset = 0

  , ctxTextAlign = TextStart
  , ctxTextBaseline = TextAlphabetic
  , ctxFont = "10px sans-serif"
  }

data Matrix = Matrix
  Double Double Double
  Double Double Double
identityMatrix :: Matrix
identityMatrix = Matrix
  1 0 0
  0 1 0
translateMatrix :: Double -> Double -> Matrix
translateMatrix x y = Matrix
  1 0 x
  0 1 y
scaleMatrix :: Double -> Double -> Matrix
scaleMatrix x y = Matrix
  x 0 0
  0 y 0
rotateMatrix :: Double -> Matrix
rotateMatrix theta = Matrix
  c (-s) 0
  s c 0
  where
    c = cos theta
    s = sin theta

instance Monoid Matrix where
  mempty = identityMatrix
  mappend
    ( Matrix
      a11 a12 a13
      a21 a22 a23
    )
    ( Matrix
      b11 b12 b13
      b21 b22 b23
    )
    = Matrix
        (a11*b11 + a12*b21) (a11*b12 + a12*b22) (a11*b13 + a12*b23 + a13)
        (a21*b11 + a22*b21) (a21*b12 + a22*b22) (a21*b13 + a22*b23 + a23)

data Point = Point Double Double deriving Show
data Size = Size Double Double deriving Show

data LinkPoint = LinkPoint Point Cost deriving Show
type Cost = Double

pointDistance :: Point -> Point -> Double
pointDistance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

movePoint :: Matrix -> Point -> Point
movePoint
  ( Matrix
    a11 a12 a13
    a21 a22 a23
  )
  ( Point x y )
  =
    Point
      (a11*x + a12*y + a13)
      (a21*x + a22*y + a23)

interpolatePoint :: Point -> Point -> Double -> Point
interpolatePoint (Point x1 y1) (Point x2 y2) ratio =
  Point
    (x1 + (x2 - x1) * ratio)
    (y1 + (y2 - y1) * ratio)

--data LinkLine :: * where
  --LinkLine :: (Linkable a, Linkable b) => a -> b -> LinkLine

-------------------------------
-- manipulate ContextState
--

setTransform :: Matrix -> Builder info ContextState draw ()
setTransform val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTransform = val} bld ()
getTransform :: Builder info ContextState draw Matrix
getTransform = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxTransform ctx)
applyTransform :: Matrix -> Builder info ContextState draw ()
applyTransform val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTransform = ctxTransform ctx <> val} bld ()

setSize :: Size -> Builder info ContextState draw ()
setSize val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxSize = val} bld ()
getSize :: Builder info ContextState draw Size
getSize = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxSize ctx)


setFill :: Maybe String -> Builder info ContextState draw ()
setFill val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxFill = val} bld ()
getFill :: Builder info ContextState draw (Maybe String)
getFill = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxFill ctx)

setStroke :: Maybe String -> Builder info ContextState draw ()
setStroke val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxStroke = val} bld ()
getStroke :: Builder info ContextState draw (Maybe String)
getStroke = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxStroke ctx)


setLineWidth :: Double -> Builder info ContextState draw ()
setLineWidth val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineWidth = val} bld ()
getLineWidth :: Builder info ContextState draw Double
getLineWidth = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineWidth ctx)

setLineCap :: LineCap -> Builder info ContextState draw ()
setLineCap val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineCap = val} bld ()
getLineCap :: Builder info ContextState draw LineCap
getLineCap = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineCap ctx)

setLineJoin :: LineJoin -> Builder info ContextState draw ()
setLineJoin val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineJoin = val} bld ()
getLineJoin :: Builder info ContextState draw LineJoin
getLineJoin = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineJoin ctx)

setMiterLimit :: Double -> Builder info ContextState draw ()
setMiterLimit val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxMiterLimit = val} bld ()
getMiterLimit :: Builder info ContextState draw Double
getMiterLimit = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxMiterLimit ctx)

setLineDash :: [Double] -> Builder info ContextState draw ()
setLineDash val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineDash = val} bld ()
getLineDash :: Builder info ContextState draw [Double]
getLineDash = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineDash ctx)

setLineDashOffset :: Double -> Builder info ContextState draw ()
setLineDashOffset val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineDashOffset = val} bld ()
getLineDashOffset :: Builder info ContextState draw Double
getLineDashOffset = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineDashOffset ctx)


setTextAlign :: TextAlign -> Builder info ContextState draw ()
setTextAlign val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTextAlign = val} bld ()
getTextAlign :: Builder info ContextState draw TextAlign
getTextAlign = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxTextAlign ctx)

setTextBaseline :: TextBaseline -> Builder info ContextState draw ()
setTextBaseline val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTextBaseline = val} bld ()
getTextBaseline :: Builder info ContextState draw TextBaseline
getTextBaseline = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxTextBaseline ctx)

setFont :: String -> Builder info ContextState draw ()
setFont val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxFont = val} bld ()
getFont :: Builder info ContextState draw String
getFont = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxFont ctx)
