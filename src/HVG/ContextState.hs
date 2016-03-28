module HVG.ContextState where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import HVG.Type

-------------------------------
-- manipulate ContextState
--

setTransform :: Matrix -> Builder ()
setTransform val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTransform = val} bld ()
getTransform :: Builder Matrix
getTransform = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxTransform ctx)
applyTransform :: Matrix -> Builder ()
applyTransform val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTransform = val <> ctxTransform ctx} bld ()


setFill :: Maybe String -> Builder ()
setFill val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxFill = val} bld ()
getFill :: Builder (Maybe String)
getFill = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxFill ctx)

setStroke :: Maybe String -> Builder ()
setStroke val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxStroke = val} bld ()
getStroke :: Builder (Maybe String)
getStroke = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxStroke ctx)


setLineWidth :: Double -> Builder ()
setLineWidth val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineWidth = val} bld ()
getLineWidth :: Builder Double
getLineWidth = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineWidth ctx)

setLineCap :: LineCap -> Builder ()
setLineCap val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineCap = val} bld ()
getLineCap :: Builder LineCap
getLineCap = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineCap ctx)

setLineJoin :: LineJoin -> Builder ()
setLineJoin val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineJoin = val} bld ()
getLineJoin :: Builder LineJoin
getLineJoin = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineJoin ctx)

setMiterLimit :: Double -> Builder ()
setMiterLimit val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxMiterLimit = val} bld ()
getMiterLimit :: Builder Double
getMiterLimit = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxMiterLimit ctx)

setLineDash :: [Double] -> Builder ()
setLineDash val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineDash = val} bld ()
getLineDash :: Builder [Double]
getLineDash = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineDash ctx)

setLineDashOffset :: Double -> Builder ()
setLineDashOffset val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineDashOffset = val} bld ()
getLineDashOffset :: Builder Double
getLineDashOffset = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineDashOffset ctx)


setTextAlign :: TextAlign -> Builder ()
setTextAlign val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTextAlign = val} bld ()
getTextAlign :: Builder TextAlign
getTextAlign = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxTextAlign ctx)

setTextBaseline :: TextBaseline -> Builder ()
setTextBaseline val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTextBaseline = val} bld ()
getTextBaseline :: Builder TextBaseline
getTextBaseline = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxTextBaseline ctx)

setFont :: String -> Builder ()
setFont val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxFont = val} bld ()
getFont :: Builder String
getFont = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxFont ctx)


name :: String -> Builder ()
name nextName = Builder $ \ctx bld ->
  BuilderPartDone
    ctx
      { ctxNextDrawName = Just nextName
      , ctxNextLinkName = Just nextName
      }
    bld
    ()

addDraw :: Draw -> Builder ()
addDraw draw = Builder $ \ctx bld ->
  case ctxNextDrawName ctx of
    Nothing ->
      BuilderPartDone
        ctx
        bld{ bldDraw = bldDraw bld >> draw }
        ()

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
          Just ctxdBlds ->
            go bld' ctxdBlds
            where
              go bld' (ContextedBuilder continue : otherCtxdBlds) =
                case continue bld' of
                  BuilderPartDone ctx'' bld'' _ ->
                    go bld'' otherCtxdBlds
                  BuilderPartWaitDraw drawName bld'' ctxdBld ->
                    go (addBuilderWaitDraw drawName ctxdBld bld'') otherCtxdBlds
                  BuilderPartWaitLink linkName bld'' ctxdBld ->
                    go (addBuilderWaitLink linkName ctxdBld bld'') otherCtxdBlds

              go bld' _ =
                bld'

      in
        BuilderPartDone ctx{ctxNextDrawName = Nothing} bld'' ()

addLink :: Link -> Builder ()
addLink link = Builder $ \ctx bld ->
  case ctxNextLinkName ctx of
    Nothing ->
      BuilderPartDone ctx bld ()

    Just myName ->
      let
        bld' = bld
          { bldNamedLink = M.insert myName link (bldNamedLink bld)
          , bldWaitLink = M.delete myName (bldWaitLink bld)
          }

        bld'' = case M.lookup myName (bldWaitLink bld) of
          Nothing ->
            bld'
          Just ctxdBlds ->
            go bld' ctxdBlds
            where
              go bld' (ContextedBuilder continue : otherCtxdBlds) =
                go (suspendBuilderPartWait (continue bld')) otherCtxdBlds

              go bld' _ =
                bld'

      in
        BuilderPartDone ctx{ctxNextLinkName = Nothing} bld'' ()

queryDraw :: String -> Builder ()
queryDraw drawName = undefined

queryLink :: String -> Builder Link
queryLink linkName = undefined

--rotate :: Double -> Builder ()
--rotate theta = applyTransform (
