module HVG.Type where

import Data.Monoid
import qualified Data.Map.Strict as M

type Link = [LinkPoint]

instance Monoid (IO ()) where
  mempty = return ()
  mappend = (>>)

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

data BuilderState info ctx draw = BuilderState
  { bldNamedInfo :: M.Map String info
  , bldWaitInfo :: M.Map String [ContextedWaitInfoBuilder info ctx draw ()]
  , bldDraw :: draw
  }
initBuilderState :: Monoid draw => BuilderState info ctx draw
initBuilderState = BuilderState
  { bldNamedInfo = M.empty
  , bldWaitInfo = M.empty
  , bldDraw = mempty
  }

addBuilderWaitInfo :: String -> ContextedWaitInfoBuilder info ctx draw () -> BuilderState info ctx draw -> BuilderState info ctx draw
addBuilderWaitInfo infoName ctxdBld bld = bld
  { bldWaitInfo = M.insertWith (++) infoName [ctxdBld] (bldWaitInfo bld)
  }

data BuilderPart info ctx draw a
  = BuilderPartDone (Maybe String) ctx (BuilderState info ctx draw) a
  | BuilderPartWaitInfo String (BuilderState info ctx draw) (ContextedWaitInfoBuilder info ctx draw a)

mapBuilderPart :: (Maybe String -> ctx -> BuilderState info ctx draw -> a -> BuilderPart info ctx draw b) -> BuilderPart info ctx draw a -> BuilderPart info ctx draw b
mapBuilderPart f = go
  where
  go = \case
    BuilderPartDone nextName ctx bld a -> f nextName ctx bld a
    BuilderPartWaitInfo infoName bld (ContextedWaitInfoBuilder ctxdAAct) ->
      BuilderPartWaitInfo infoName bld $ ContextedWaitInfoBuilder $ \link bld' -> go (ctxdAAct link bld')

forBuilderPart :: BuilderPart info ctx draw a -> (Maybe String -> ctx -> BuilderState info ctx draw -> a -> BuilderPart info ctx draw b) -> BuilderPart info ctx draw b
forBuilderPart = flip mapBuilderPart

suspendBuilderPartWait :: BuilderPart info ctx draw () -> BuilderState info ctx draw
suspendBuilderPartWait = \case
  BuilderPartDone _ _ bld' _ ->
    bld'
  BuilderPartWaitInfo infoName bld' ctxdBld ->
    addBuilderWaitInfo infoName ctxdBld bld'

newtype Builder info ctx draw a = Builder (Maybe String -> ctx -> BuilderState info ctx draw -> BuilderPart info ctx draw a)
newtype ContextedWaitInfoBuilder info ctx draw a = ContextedWaitInfoBuilder (info -> BuilderState info ctx draw -> BuilderPart info ctx draw a)

fork :: Builder info ctx draw () -> Builder info ctx draw ()
fork (Builder act) = Builder $ \nextName ctx bld ->
  BuilderPartDone
    Nothing
    ctx
    (suspendBuilderPartWait (act nextName ctx bld))
    ()

local :: Builder info ctx draw a -> Builder info ctx draw a
local (Builder act) = Builder $ \nextName ctx bld ->
  forBuilderPart (act nextName ctx bld) $ \_ _ bld' a ->
    BuilderPartDone Nothing ctx bld' a

instance Functor (Builder info ctx draw) where
  fmap f (Builder act) = Builder $ \nextName ctx bld ->
    fmap f (act nextName ctx bld)
instance Functor (ContextedWaitInfoBuilder info ctx draw) where
  fmap f (ContextedWaitInfoBuilder act) = ContextedWaitInfoBuilder $ \link bld ->
    fmap f (act link bld)
instance Functor (BuilderPart info ctx draw) where
  fmap f = \case
    BuilderPartDone nextName ctx bld a -> BuilderPartDone nextName ctx bld (f a)
    BuilderPartWaitInfo infoName bld ctxdBuilder -> BuilderPartWaitInfo infoName bld (fmap f ctxdBuilder)

instance Applicative (Builder info ctx draw) where
  pure a = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld a
  Builder fAct <*> Builder aAct = Builder $ \nextName ctx bld ->
    forBuilderPart (fAct nextName ctx bld) $ \nextName' ctx' bld' f ->
      f <$> aAct nextName' ctx' bld'

instance Monad (Builder info ctx draw) where
  Builder mAct >>= f = Builder $ \nextName ctx bld ->
    forBuilderPart (mAct nextName ctx bld) $ \nextName' ctx' bld' a ->
      let
        Builder fAct = f a
      in
        fAct nextName' ctx' bld'

name :: String -> Builder info ctx draw ()
name nextName = Builder $ \nextName' ctx bld ->
  BuilderPartDone
    (Just nextName)
    ctx
    bld
    ()

addDraw :: Monoid draw => draw -> Builder info ctx draw ()
addDraw draw = Builder $ \nextName ctx bld ->
  BuilderPartDone
    nextName
    ctx
    bld{ bldDraw = bldDraw bld <> draw }
    ()

addInfo :: info -> Builder info ctx draw ()
addInfo info = Builder $ \nextName ctx bld ->
  case nextName of
    Nothing ->
      BuilderPartDone
        nextName
        ctx
        bld
        ()

    Just myName ->
      let
        bld' = bld
          { bldNamedInfo = M.insert myName info (bldNamedInfo bld)
          , bldWaitInfo = M.delete myName (bldWaitInfo bld)
          }

        bld'' = case M.lookup myName (bldWaitInfo bld) of
          Nothing ->
            bld'
          Just ctxdBlds ->
            go bld' ctxdBlds
            where
              go bld' (ContextedWaitInfoBuilder continue : otherCtxdBlds) =
                go (suspendBuilderPartWait (continue info bld')) otherCtxdBlds

              go bld' _ =
                bld'

      in
        BuilderPartDone Nothing ctx bld'' ()

queryInfo :: String -> Builder info ctx draw info
queryInfo infoName = Builder $ \nextName ctx bld ->
  case M.lookup infoName (bldNamedInfo bld) of
    Just info ->
      BuilderPartDone nextName ctx bld info
    Nothing ->
      --error $ infoName ++ " " ++ show (map fst $ M.toList $ bldNamedInfo bld)
      BuilderPartWaitInfo infoName bld $ ContextedWaitInfoBuilder $ \info bld' ->
        BuilderPartDone nextName ctx bld' info

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
