module HVG
    ( 
    ) where

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Context a -> IO a
drawCanvas canvasCSSQuery (Context draws) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  (_, a) <- draws initContext
  putStrLn   "})();"
  return a

