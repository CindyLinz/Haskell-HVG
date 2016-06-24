module HVG.ContextState where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import HVG.Type

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
