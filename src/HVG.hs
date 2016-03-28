module HVG
    ( 
    ) where

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Builder (IO a) -> IO ()
drawCanvas canvasCSSQuery (Builder drawBuilder) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  let BuilderPartDone _ bld _ = drawBuilder initContextState initBuilderState
  bldDraw bld
  putStrLn   "})();"

