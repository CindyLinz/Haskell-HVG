module HVG
    ( drawCanvas
    ) where

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Size -> Builder () -> IO ()
drawCanvas canvasCSSQuery size (Builder drawBuilder) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ show canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  let BuilderPartDone _ bld _ = drawBuilder (initContextState size) initBuilderState
  bldDraw bld
  putStrLn   "})();"

