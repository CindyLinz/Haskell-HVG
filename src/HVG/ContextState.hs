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

putTransform :: Matrix -> Builder info ContextState draw ()
putTransform val = modifyStructState $ \ctx -> ctx{ctxTransform = val}
getTransform :: Builder info ContextState draw Matrix
getTransform = ctxTransform <$> getStructState
applyTransform :: Matrix -> Builder info ContextState draw ()
applyTransform val = modifyStructState $ \ctx -> ctx{ctxTransform = ctxTransform ctx <> val}

setSize :: Size -> Builder info ContextState draw ()
setSize val = modifyStructState $ \ctx -> ctx{ctxSize = val}
getSize :: Builder info ContextState draw Size
getSize = ctxSize <$> getStructState


setFill :: Maybe String -> Builder info ContextState draw ()
setFill val = modifyStructState $ \ctx -> ctx{ctxFill = val}
getFill :: Builder info ContextState draw (Maybe String)
getFill = ctxFill <$> getStructState

setStroke :: Maybe String -> Builder info ContextState draw ()
setStroke val = modifyStructState $ \ctx -> ctx{ctxStroke = val}
getStroke :: Builder info ContextState draw (Maybe String)
getStroke = ctxStroke <$> getStructState


setLineWidth :: Double -> Builder info ContextState draw ()
setLineWidth val = modifyStructState $ \ctx -> ctx{ctxLineWidth = val}
getLineWidth :: Builder info ContextState draw Double
getLineWidth = ctxLineWidth <$> getStructState

setLineCap :: LineCap -> Builder info ContextState draw ()
setLineCap val = modifyStructState $ \ctx -> ctx{ctxLineCap = val}
getLineCap :: Builder info ContextState draw LineCap
getLineCap = ctxLineCap <$> getStructState

setLineJoin :: LineJoin -> Builder info ContextState draw ()
setLineJoin val = modifyStructState $ \ctx -> ctx{ctxLineJoin = val}
getLineJoin :: Builder info ContextState draw LineJoin
getLineJoin = ctxLineJoin <$> getStructState

setMiterLimit :: Double -> Builder info ContextState draw ()
setMiterLimit val = modifyStructState $ \ctx -> ctx{ctxMiterLimit = val}
getMiterLimit :: Builder info ContextState draw Double
getMiterLimit = ctxMiterLimit <$> getStructState

setLineDash :: [Double] -> Builder info ContextState draw ()
setLineDash val = modifyStructState $ \ctx -> ctx{ctxLineDash = val}
getLineDash :: Builder info ContextState draw [Double]
getLineDash = ctxLineDash <$> getStructState

setLineDashOffset :: Double -> Builder info ContextState draw ()
setLineDashOffset val = modifyStructState $ \ctx -> ctx{ctxLineDashOffset = val}
getLineDashOffset :: Builder info ContextState draw Double
getLineDashOffset = ctxLineDashOffset <$> getStructState


setTextAlign :: TextAlign -> Builder info ContextState draw ()
setTextAlign val = modifyStructState $ \ctx -> ctx{ctxTextAlign = val}
getTextAlign :: Builder info ContextState draw TextAlign
getTextAlign = ctxTextAlign <$> getStructState

setTextBaseline :: TextBaseline -> Builder info ContextState draw ()
setTextBaseline val = modifyStructState $ \ctx -> ctx{ctxTextBaseline = val}
getTextBaseline :: Builder info ContextState draw TextBaseline
getTextBaseline = ctxTextBaseline <$> getStructState

setFont :: String -> Builder info ContextState draw ()
setFont val = modifyStructState $ \ctx -> ctx{ctxFont = val}
getFont :: Builder info ContextState draw String
getFont = ctxFont <$> getStructState
