module HVG.Type where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

  , ctxNextDrawName :: Maybe String
  , ctxNextLinkName :: Maybe String
  }
initContextState :: ContextState
initContextState = ContextState
  { ctxTransform = identityMatrix

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

  , ctxNextDrawName = Nothing
  , ctxNextLinkName = Nothing
  }

data BuilderState = BuilderState
  { bldNamedDraw :: S.Set String
  , bldNamedLink :: M.Map String Link
  , bldWaitDraw :: M.Map String (ContextedBuilder ())
  , bldWaitLink :: M.Map String (ContextedBuilder ())
  , bldDraw :: IO ()
  }
initBuilderState :: BuilderState
initBuilderState = BuilderState
  { bldNamedDraw = S.empty
  , bldNamedLink = M.empty
  , bldWaitDraw = M.empty
  , bldWaitLink = M.empty
  , bldDraw = return ()
  }

data Builder a = Builder (ContextState -> BuilderState -> (ContextState, BuilderState, a))
data ContextedBuilder a = ContextedBuilder (BuilderState -> (ContextState, BuilderState, a))

instance Functor Builder where
  fmap f (Builder act) = Builder $ \ctx bld ->
    let
      (ctx', bld', a) = act ctx bld
    in
      (ctx', bld', f a)

instance Applicative Builder where
  pure a = Builder $ \ctx bld -> (ctx, bld, a)
  Builder fAct <*> Builder aAct = Builder $ \ctx bld ->
    let
      (ctx', bld', f) = fAct ctx bld
      (ctx'', bld'', a) = aAct ctx' bld'
    in
      (ctx'', bld'', f a)

instance Monad Builder where
  Builder mAct >>= f = Builder $ \ctx bld ->
    let
      (ctx', bld', a) = mAct ctx bld
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

data Point = Point Double Double
data Size = Size Double Double

data LinkPoint = LinkPoint Point Cost
type Cost = Double

--data LinkLine :: * where
  --LinkLine :: (Linkable a, Linkable b) => a -> b -> LinkLine
