module HVG.ContextState where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import HVG.Type

-------------------------------
-- manipulate ContextState
--

setTransform :: Matrix -> Builder ()
setTransform val = Builder $ \ctx bld -> (ctx {ctxTransform = val}, bld, ())
getTransform :: Builder Matrix
getTransform = Builder $ \ctx bld -> (ctx, bld, ctxTransform ctx)
applyTransform :: Matrix -> Builder ()
applyTransform val = Builder $ \ctx bld -> (ctx {ctxTransform = val <> ctxTransform ctx}, bld, ())


setFill :: Maybe String -> Builder ()
setFill val = Builder $ \ctx bld -> (ctx {ctxFill = val}, bld, ())
getFill :: Builder (Maybe String)
getFill = Builder $ \ctx bld -> (ctx, bld, ctxFill ctx)

setStroke :: Maybe String -> Builder ()
setStroke val = Builder $ \ctx bld -> (ctx {ctxStroke = val}, bld, ())
getStroke :: Builder (Maybe String)
getStroke = Builder $ \ctx bld -> (ctx, bld, ctxStroke ctx)


setLineWidth :: Double -> Builder ()
setLineWidth val = Builder $ \ctx bld -> (ctx {ctxLineWidth = val}, bld, ())
getLineWidth :: Builder Double
getLineWidth = Builder $ \ctx bld -> (ctx, bld, ctxLineWidth ctx)

setLineCap :: LineCap -> Builder ()
setLineCap val = Builder $ \ctx bld -> (ctx {ctxLineCap = val}, bld, ())
getLineCap :: Builder LineCap
getLineCap = Builder $ \ctx bld -> (ctx, bld, ctxLineCap ctx)

setLineJoin :: LineJoin -> Builder ()
setLineJoin val = Builder $ \ctx bld -> (ctx {ctxLineJoin = val}, bld, ())
getLineJoin :: Builder LineJoin
getLineJoin = Builder $ \ctx bld -> (ctx, bld, ctxLineJoin ctx)

setMiterLimit :: Double -> Builder ()
setMiterLimit val = Builder $ \ctx bld -> (ctx {ctxMiterLimit = val}, bld, ())
getMiterLimit :: Builder Double
getMiterLimit = Builder $ \ctx bld -> (ctx, bld, ctxMiterLimit ctx)

setLineDash :: [Double] -> Builder ()
setLineDash val = Builder $ \ctx bld -> (ctx {ctxLineDash = val}, bld, ())
getLineDash :: Builder [Double]
getLineDash = Builder $ \ctx bld -> (ctx, bld, ctxLineDash ctx)

setLineDashOffset :: Double -> Builder ()
setLineDashOffset val = Builder $ \ctx bld -> (ctx {ctxLineDashOffset = val}, bld, ())
getLineDashOffset :: Builder Double
getLineDashOffset = Builder $ \ctx bld -> (ctx, bld, ctxLineDashOffset ctx)


setTextAlign :: TextAlign -> Builder ()
setTextAlign val = Builder $ \ctx bld -> (ctx {ctxTextAlign = val}, bld, ())
getTextAlign :: Builder TextAlign
getTextAlign = Builder $ \ctx bld -> (ctx, bld, ctxTextAlign ctx)

setTextBaseline :: TextBaseline -> Builder ()
setTextBaseline val = Builder $ \ctx bld -> (ctx {ctxTextBaseline = val}, bld, ())
getTextBaseline :: Builder TextBaseline
getTextBaseline = Builder $ \ctx bld -> (ctx, bld, ctxTextBaseline ctx)

setFont :: String -> Builder ()
setFont val = Builder $ \ctx bld -> (ctx {ctxFont = val}, bld, ())
getFont :: Builder String
getFont = Builder $ \ctx bld -> (ctx, bld, ctxFont ctx)


local :: Builder a -> Builder a
local (Builder act) = Builder $ \ctx bld ->
  let
    (_, bld', a) = act ctx bld
  in
    (ctx, bld', a)

name :: String -> Builder ()
name nextName = Builder $ \ctx bld ->
  ( ctx
    { ctxNextDrawName = Just nextName
    , ctxNextLinkName = Just nextName
    }
  , bld
  , ()
  )

addDraw :: Draw -> Builder ()
addDraw draw = Builder $ \ctx bld ->
  case ctxNextDrawName ctx of
    Nothing ->
      (ctx, bld, ())

    Just myName ->
      let
        bld' = bld
          { bldNamedDraw = S.insert myName (bldNamedDraw bld)
          , bldWaitDraw = M.delete myName (bldWaitDraw bld)
          , bldDraw = bldDraw bld >> draw
          }

        bld'' = case M.lookup myName (bldWaitDraw bld) of
          Nothing ->
            bld'
          Just (ContextedBuilder continue) ->
            bld'' where (_, bld'', ()) = continue bld'

      in
        (ctx {ctxNextDrawName = Nothing}, bld'', ())

addLink :: Link -> Builder ()
addLink link = Builder $ \ctx bld ->
  case ctxNextLinkName ctx of
    Nothing ->
      (ctx, bld, ())

    Just myName ->
      let
        bld' = bld
          { bldNamedLink = M.insert myName link (bldNamedLink bld)
          , bldWaitLink = M.delete myName (bldWaitLink bld)
          }

        bld'' = case M.lookup myName (bldWaitLink bld) of
          Nothing ->
            bld'
          Just (ContextedBuilder continue) ->
            bld'' where (_, bld'', ()) = continue bld'

      in
        (ctx {ctxNextLinkName = Nothing}, bld'', ())

queryDraw :: String -> Builder ()
queryDraw drawName = undefined

queryLink :: String -> Builder Link
queryLink linkName = undefined

--rotate :: Double -> Builder ()
--rotate theta = applyTransform (
