module HVG.ContextState where

import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import HVG.Type

-------------------------------
-- manipulate ContextState
--

setTransform :: Matrix -> Builder info ()
setTransform val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTransform = val} bld ()
getTransform :: Builder info Matrix
getTransform = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxTransform ctx)
applyTransform :: Matrix -> Builder info ()
applyTransform val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTransform = ctxTransform ctx <> val} bld ()

setSize :: Size -> Builder info ()
setSize val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxSize = val} bld ()
getSize :: Builder info Size
getSize = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxSize ctx)


setFill :: Maybe String -> Builder info ()
setFill val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxFill = val} bld ()
getFill :: Builder info (Maybe String)
getFill = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxFill ctx)

setStroke :: Maybe String -> Builder info ()
setStroke val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxStroke = val} bld ()
getStroke :: Builder info (Maybe String)
getStroke = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxStroke ctx)


setLineWidth :: Double -> Builder info ()
setLineWidth val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineWidth = val} bld ()
getLineWidth :: Builder info Double
getLineWidth = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineWidth ctx)

setLineCap :: LineCap -> Builder info ()
setLineCap val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineCap = val} bld ()
getLineCap :: Builder info LineCap
getLineCap = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineCap ctx)

setLineJoin :: LineJoin -> Builder info ()
setLineJoin val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineJoin = val} bld ()
getLineJoin :: Builder info LineJoin
getLineJoin = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineJoin ctx)

setMiterLimit :: Double -> Builder info ()
setMiterLimit val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxMiterLimit = val} bld ()
getMiterLimit :: Builder info Double
getMiterLimit = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxMiterLimit ctx)

setLineDash :: [Double] -> Builder info ()
setLineDash val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineDash = val} bld ()
getLineDash :: Builder info [Double]
getLineDash = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineDash ctx)

setLineDashOffset :: Double -> Builder info ()
setLineDashOffset val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxLineDashOffset = val} bld ()
getLineDashOffset :: Builder info Double
getLineDashOffset = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxLineDashOffset ctx)


setTextAlign :: TextAlign -> Builder info ()
setTextAlign val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTextAlign = val} bld ()
getTextAlign :: Builder info TextAlign
getTextAlign = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxTextAlign ctx)

setTextBaseline :: TextBaseline -> Builder info ()
setTextBaseline val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxTextBaseline = val} bld ()
getTextBaseline :: Builder info TextBaseline
getTextBaseline = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxTextBaseline ctx)

setFont :: String -> Builder info ()
setFont val = Builder $ \ctx bld -> BuilderPartDone ctx{ctxFont = val} bld ()
getFont :: Builder info String
getFont = Builder $ \ctx bld -> BuilderPartDone ctx bld (ctxFont ctx)


name :: String -> Builder info ()
name nextName = Builder $ \ctx bld ->
  BuilderPartDone
    ctx{ ctxNextInfoName = Just nextName }
    bld
    ()
{-
addDraw :: Draw -> Builder info ()
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
              go bld' (ContextedWaitDrawBuilder continue : otherCtxdBlds) =
                go (suspendBuilderPartWait (continue bld')) otherCtxdBlds

              go bld' _ =
                bld'

      in
        BuilderPartDone ctx{ctxNextDrawName = Nothing} bld'' ()
-}

addEntity :: info -> Draw -> Builder info ()
addEntity info draw = Builder $ \ctx bld ->
  case ctxNextInfoName ctx of
    Nothing ->
      BuilderPartDone
        ctx
        bld{ bldDraw = bldDraw bld >> draw }
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
        BuilderPartDone ctx{ctxNextInfoName = Nothing} bld'' ()

{-
queryDraw :: String -> Builder info ()
queryDraw drawName = Builder $ \ctx bld ->
  if S.member drawName (bldNamedDraw bld) then
    BuilderPartDone ctx bld ()
  else
    BuilderPartWaitDraw drawName bld $ ContextedWaitDrawBuilder $ \bld' ->
      BuilderPartDone ctx bld' ()
-}

queryInfo :: String -> Builder info info
queryInfo infoName = Builder $ \ctx bld ->
  case M.lookup infoName (bldNamedInfo bld) of
    Just info ->
      BuilderPartDone ctx bld info
    Nothing ->
      --error $ infoName ++ " " ++ show (map fst $ M.toList $ bldNamedInfo bld)
      BuilderPartWaitInfo infoName bld $ ContextedWaitInfoBuilder $ \info bld' ->
        BuilderPartDone ctx bld' info
