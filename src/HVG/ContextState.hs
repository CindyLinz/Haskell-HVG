module HVG.ContextState where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import HVG.Type

-------------------------------
-- manipulate ContextState
--

setTransform :: Matrix -> Builder info ContextState ()
setTransform val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTransform = val} bld ()
getTransform :: Builder info ContextState Matrix
getTransform = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxTransform ctx)
applyTransform :: Matrix -> Builder info ContextState ()
applyTransform val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTransform = ctxTransform ctx <> val} bld ()

setSize :: Size -> Builder info ContextState ()
setSize val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxSize = val} bld ()
getSize :: Builder info ContextState Size
getSize = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxSize ctx)


setFill :: Maybe String -> Builder info ContextState ()
setFill val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxFill = val} bld ()
getFill :: Builder info ContextState (Maybe String)
getFill = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxFill ctx)

setStroke :: Maybe String -> Builder info ContextState ()
setStroke val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxStroke = val} bld ()
getStroke :: Builder info ContextState (Maybe String)
getStroke = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxStroke ctx)


setLineWidth :: Double -> Builder info ContextState ()
setLineWidth val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineWidth = val} bld ()
getLineWidth :: Builder info ContextState Double
getLineWidth = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineWidth ctx)

setLineCap :: LineCap -> Builder info ContextState ()
setLineCap val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineCap = val} bld ()
getLineCap :: Builder info ContextState LineCap
getLineCap = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineCap ctx)

setLineJoin :: LineJoin -> Builder info ContextState ()
setLineJoin val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineJoin = val} bld ()
getLineJoin :: Builder info ContextState LineJoin
getLineJoin = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineJoin ctx)

setMiterLimit :: Double -> Builder info ContextState ()
setMiterLimit val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxMiterLimit = val} bld ()
getMiterLimit :: Builder info ContextState Double
getMiterLimit = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxMiterLimit ctx)

setLineDash :: [Double] -> Builder info ContextState ()
setLineDash val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineDash = val} bld ()
getLineDash :: Builder info ContextState [Double]
getLineDash = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineDash ctx)

setLineDashOffset :: Double -> Builder info ContextState ()
setLineDashOffset val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxLineDashOffset = val} bld ()
getLineDashOffset :: Builder info ContextState Double
getLineDashOffset = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxLineDashOffset ctx)


setTextAlign :: TextAlign -> Builder info ContextState ()
setTextAlign val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTextAlign = val} bld ()
getTextAlign :: Builder info ContextState TextAlign
getTextAlign = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxTextAlign ctx)

setTextBaseline :: TextBaseline -> Builder info ContextState ()
setTextBaseline val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxTextBaseline = val} bld ()
getTextBaseline :: Builder info ContextState TextBaseline
getTextBaseline = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxTextBaseline ctx)

setFont :: String -> Builder info ContextState ()
setFont val = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx{ctxFont = val} bld ()
getFont :: Builder info ContextState String
getFont = Builder $ \nextName ctx bld -> BuilderPartDone nextName ctx bld (ctxFont ctx)
