module HVG.Type where

import Data.Monoid
import qualified Data.Map.Strict as M

type Draw = IO ()
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

  , ctxNextInfoName :: Maybe String
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

  , ctxNextInfoName = Nothing
  }

data BuilderState info = BuilderState
  { bldNamedInfo :: M.Map String info
  , bldWaitInfo :: M.Map String [ContextedWaitInfoBuilder info ()]
  , bldDraw :: IO ()
  }
initBuilderState :: BuilderState info
initBuilderState = BuilderState
  { bldNamedInfo = M.empty
  , bldWaitInfo = M.empty
  , bldDraw = return ()
  }

addBuilderWaitInfo :: String -> ContextedWaitInfoBuilder info () -> BuilderState info -> BuilderState info
addBuilderWaitInfo infoName ctxdBld bld = bld
  { bldWaitInfo = M.insertWith (++) infoName [ctxdBld] (bldWaitInfo bld)
  }

data BuilderPart info a
  = BuilderPartDone ContextState (BuilderState info) a
  | BuilderPartWaitInfo String (BuilderState info) (ContextedWaitInfoBuilder info a)

mapBuilderPart :: (ContextState -> BuilderState info -> a -> BuilderPart info b) -> BuilderPart info a -> BuilderPart info b
mapBuilderPart f = go
  where
  go = \case
    BuilderPartDone ctx bld a -> f ctx bld a
    BuilderPartWaitInfo infoName bld (ContextedWaitInfoBuilder ctxdAAct) ->
      BuilderPartWaitInfo infoName bld $ ContextedWaitInfoBuilder $ \link bld' -> go (ctxdAAct link bld')

forBuilderPart :: BuilderPart info a -> (ContextState -> BuilderState info -> a -> BuilderPart info b) -> BuilderPart info b
forBuilderPart = flip mapBuilderPart

suspendBuilderPartWait :: BuilderPart info () -> BuilderState info
suspendBuilderPartWait = \case
  BuilderPartDone _ bld' _ ->
    bld'
  BuilderPartWaitInfo infoName bld' ctxdBld ->
    addBuilderWaitInfo infoName ctxdBld bld'

newtype Builder info a = Builder (ContextState -> BuilderState info -> BuilderPart info a)
newtype ContextedWaitInfoBuilder info a = ContextedWaitInfoBuilder (info -> BuilderState info -> BuilderPart info a)

fork :: Builder info () -> Builder info ()
fork (Builder act) = Builder $ \ctx bld ->
  BuilderPartDone
    ctx{ctxNextInfoName=Nothing}
    (suspendBuilderPartWait (act ctx bld))
    ()

local :: Builder info a -> Builder info a
local (Builder act) = Builder $ \ctx bld ->
  forBuilderPart (act ctx bld) $ \_ bld' a ->
    BuilderPartDone ctx{ctxNextInfoName=Nothing} bld' a

instance Functor (Builder info) where
  fmap f (Builder act) = Builder $ \ctx bld ->
    fmap f (act ctx bld)
instance Functor (ContextedWaitInfoBuilder info) where
  fmap f (ContextedWaitInfoBuilder act) = ContextedWaitInfoBuilder $ \link bld ->
    fmap f (act link bld)
instance Functor (BuilderPart info) where
  fmap f = \case
    BuilderPartDone ctx bld a -> BuilderPartDone ctx bld (f a)
    BuilderPartWaitInfo infoName bld ctxdBuilder -> BuilderPartWaitInfo infoName bld (fmap f ctxdBuilder)

instance Applicative (Builder info) where
  pure a = Builder $ \ctx bld -> BuilderPartDone ctx bld a
  Builder fAct <*> Builder aAct = Builder $ \ctx bld ->
    forBuilderPart (fAct ctx bld) $ \ctx' bld' f ->
      f <$> aAct ctx' bld'

instance Monad (Builder info) where
  Builder mAct >>= f = Builder $ \ctx bld ->
    forBuilderPart (mAct ctx bld) $ \ctx' bld' a ->
      let
        Builder fAct = f a
      in
        fAct ctx' bld'

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
