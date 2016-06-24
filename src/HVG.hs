module HVG
    ( drawCanvas
    ) where

import Control.Monad
import qualified Data.Map.Strict as M

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Size -> Builder info ContextState () -> IO ()
drawCanvas canvasCSSQuery size (Builder drawBuilder) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ show canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  case drawBuilder Nothing (initContextState size) initBuilderState of
    BuilderPartDone _ _ bld _ -> do
      forM_ (M.toList (bldWaitInfo bld)) $ \(infoName, _) ->
        putStrLn $ "  console.warn('wait no info: ' + " ++ show infoName ++ ")"
      bldDraw bld
    BuilderPartWaitInfo infoName _ _ ->
      putStrLn $ "  console.warn('wait no draw: ' + " ++ show infoName ++ ")"
  putStrLn   "})();"

