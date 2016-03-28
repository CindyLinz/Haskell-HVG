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
  , bldWaitDraw :: M.Map String [ContextedBuilder ()]
  , bldWaitLink :: M.Map String [ContextedBuilder ()]
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

addBuilderWaitDraw :: String -> ContextedBuilder () -> BuilderState -> BuilderState
addBuilderWaitDraw drawName ctxdBld bld = bld
  { bldWaitDraw = M.insertWith (++) drawName [ctxdBld] (bldWaitDraw bld)
  }
addBuilderWaitLink :: String -> ContextedBuilder () -> BuilderState -> BuilderState
addBuilderWaitLink linkName ctxdBld bld = bld
  { bldWaitLink = M.insertWith (++) linkName [ctxdBld] (bldWaitLink bld)
  }

newtype Builder a = Builder (ContextState -> BuilderState -> BuilderPart a)
newtype ContextedBuilder a = ContextedBuilder (BuilderState -> BuilderPart a)

data BuilderPart a
  = BuilderPartDone ContextState BuilderState a
  | BuilderPartWaitDraw String (ContextedBuilder a)
  | BuilderPartWaitLink String (ContextedBuilder a)

instance Functor Builder where
  fmap f (Builder act) = Builder $ \ctx bld ->
    fmap f (act ctx bld)
instance Functor ContextedBuilder where
  fmap f (ContextedBuilder act) = ContextedBuilder $ \bld ->
    fmap f (act bld)
instance Functor BuilderPart where
  fmap f = \case
    BuilderPartDone ctx' bld' a -> BuilderPartDone ctx' bld' (f a)
    BuilderPartWaitDraw drawName ctxdBuilder -> BuilderPartWaitDraw drawName (fmap f ctxdBuilder)
    BuilderPartWaitLink linkName ctxdBuilder -> BuilderPartWaitLink linkName (fmap f ctxdBuilder)

instance Applicative Builder where
  pure a = Builder $ \ctx bld -> BuilderPartDone ctx bld a
  Builder fAct <*> Builder aAct = Builder $ \ctx bld ->
    go (fAct ctx bld)
    where
      go = \case
        BuilderPartDone ctx' bld' f ->
          case aAct ctx' bld' of
            BuilderPartDone ctx'' bld'' a ->
              BuilderPartDone ctx'' bld'' (f a)
            BuilderPartWaitDraw drawName (ContextedBuilder ctxdAAct) ->
              BuilderPartWaitDraw drawName $ ContextedBuilder $ \bld -> f <$> ctxdAAct bld
            BuilderPartWaitLink linkName (ContextedBuilder ctxdAAct) ->
              BuilderPartWaitLink linkName $ ContextedBuilder $ \bld -> f <$> ctxdAAct bld
        BuilderPartWaitDraw drawName (ContextedBuilder ctxdFAct) ->
          BuilderPartWaitDraw drawName $ ContextedBuilder $ \bld -> go (ctxdFAct bld)
        BuilderPartWaitLink linkName (ContextedBuilder ctxdFAct) ->
          BuilderPartWaitLink linkName $ ContextedBuilder $ \bld -> go (ctxdFAct bld)
  {-
  Builder fAct <*> Builder aAct = Builder $ \ctx bld ->
    case fAct ctx bld of
      BuilderPartDone ctx' bld' f ->
        case aAct ctx' bld' of
          BuilderPartDone ctx'' bld'' a ->
            BuilderPartDone ctx'' bld'' (f a)
          BuilderPartWaitDraw drawName (ContextedBuilder ctxdAAct) ->
            BuilderPartDone 
            BuilderPartWaitDraw drawName $ ContextedBuilder $ \bld -> f <$> ctxdAAct bld
          BuilderPartWaitLink linkName (ContextedBuilder ctxdAAct) ->
            BuilderPartWaitLink linkName $ ContextedBuilder $ \bld -> f <$> ctxdAAct bld
  -}

instance Monad Builder where
  Builder mAct >>= f = Builder $ \ctx bld ->
    go (mAct ctx bld)
    where
      go = \case
        BuilderPartDone ctx' bld' a -> fAct ctx' bld'
          where Builder fAct = f a
        BuilderPartWaitDraw drawName (ContextedBuilder ctxdFAct) ->
          BuilderPartWaitDraw drawName $ ContextedBuilder $ \bld -> go (ctxdFAct bld)
        BuilderPartWaitLink linkName (ContextedBuilder ctxdFAct) ->
          BuilderPartWaitLink linkName $ ContextedBuilder $ \bld -> go (ctxdFAct bld)

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
