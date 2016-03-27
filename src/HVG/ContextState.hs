module HVG.ContextState where

import Data.Monoid

import HVG.Type

-------------------------------
-- manipulate ContextState
--

setTransform :: Matrix -> Context ()
setTransform val = Context $ \ctx -> return (ctx {ctxTransform = val}, ())
getTransform :: Context Matrix
getTransform = Context $ \ctx -> return (ctx, ctxTransform ctx)
applyTransform :: Matrix -> Context ()
applyTransform val = Context $ \ctx -> return (ctx {ctxTransform = val <> ctxTransform ctx}, ())


setFill :: Maybe String -> Context ()
setFill val = Context $ \ctx -> return (ctx {ctxFill = val}, ())
getFill :: Context (Maybe String)
getFill = Context $ \ctx -> return (ctx, ctxFill ctx)

setStroke :: Maybe String -> Context ()
setStroke val = Context $ \ctx -> return (ctx {ctxStroke = val}, ())
getStroke :: Context (Maybe String)
getStroke = Context $ \ctx -> return (ctx, ctxStroke ctx)


setLineWidth :: Double -> Context ()
setLineWidth val = Context $ \ctx -> return (ctx {ctxLineWidth = val}, ())
getLineWidth :: Context Double
getLineWidth = Context $ \ctx -> return (ctx, ctxLineWidth ctx)

setLineCap :: LineCap -> Context ()
setLineCap val = Context $ \ctx -> return (ctx {ctxLineCap = val}, ())
getLineCap :: Context LineCap
getLineCap = Context $ \ctx -> return (ctx, ctxLineCap ctx)

setLineJoin :: LineJoin -> Context ()
setLineJoin val = Context $ \ctx -> return (ctx {ctxLineJoin = val}, ())
getLineJoin :: Context LineJoin
getLineJoin = Context $ \ctx -> return (ctx, ctxLineJoin ctx)

setMiterLimit :: Double -> Context ()
setMiterLimit val = Context $ \ctx -> return (ctx {ctxMiterLimit = val}, ())
getMiterLimit :: Context Double
getMiterLimit = Context $ \ctx -> return (ctx, ctxMiterLimit ctx)

setLineDash :: [Double] -> Context ()
setLineDash val = Context $ \ctx -> return (ctx {ctxLineDash = val}, ())
getLineDash :: Context [Double]
getLineDash = Context $ \ctx -> return (ctx, ctxLineDash ctx)

setLineDashOffset :: Double -> Context ()
setLineDashOffset val = Context $ \ctx -> return (ctx {ctxLineDashOffset = val}, ())
getLineDashOffset :: Context Double
getLineDashOffset = Context $ \ctx -> return (ctx, ctxLineDashOffset ctx)


setTextAlign :: TextAlign -> Context ()
setTextAlign val = Context $ \ctx -> return (ctx {ctxTextAlign = val}, ())
getTextAlign :: Context TextAlign
getTextAlign = Context $ \ctx -> return (ctx, ctxTextAlign ctx)

setTextBaseline :: TextBaseline -> Context ()
setTextBaseline val = Context $ \ctx -> return (ctx {ctxTextBaseline = val}, ())
getTextBaseline :: Context TextBaseline
getTextBaseline = Context $ \ctx -> return (ctx, ctxTextBaseline ctx)

setFont :: String -> Context ()
setFont val = Context $ \ctx -> return (ctx {ctxFont = val}, ())
getFont :: Context String
getFont = Context $ \ctx -> return (ctx, ctxFont ctx)

