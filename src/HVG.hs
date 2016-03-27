module HVG
    ( 
    ) where

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Context (IO a) -> IO a
drawCanvas canvasCSSQuery (Context drawBuilder) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  let (_, draw) = drawBuilder initContext
  res <- draw
  putStrLn   "})();"
  return res

