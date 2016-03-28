module HVG
    ( 
    ) where

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Builder (IO a) -> IO a
drawCanvas canvasCSSQuery (Builder drawBuilder) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  let (_, _, draw) = drawBuilder initContextState initBuilderState
  res <- draw
  putStrLn   "})();"
  return res

