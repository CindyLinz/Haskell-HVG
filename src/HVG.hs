module HVG
    ( drawCanvas
    ) where

import Control.Monad
import qualified Data.Map.Strict as M

import HVG.Type
import HVG.Context2D

drawCanvas :: String -> Size -> Builder () -> IO ()
drawCanvas canvasCSSQuery size (Builder drawBuilder) = do
  putStrLn   "(function(canvas){"
  putStrLn   "  if( !document ) return;"
  putStrLn $ "  var canvas = document.querySelector(" ++ show canvasCSSQuery ++ ")";
  putStrLn $ "  if( !canvas ) return;"
  putStrLn $ "  var ctx = canvas.getContext('2d');"
  case drawBuilder (initContextState size) initBuilderState of
    BuilderPartDone _ bld _ -> do
      forM_ (M.toList (bldWaitDraw bld)) $ \(drawName, _) ->
        putStrLn $ "  console.warn('wait no draw: ' + " ++ show drawName ++ ")"
      forM_ (M.toList (bldWaitLink bld)) $ \(linkName, _) ->
        putStrLn $ "  console.warn('wait no link: ' + " ++ show linkName ++ ")"
      bldDraw bld
    BuilderPartWaitDraw drawName _ _ ->
      putStrLn $ "  console.warn('wait no link: ' + " ++ show drawName ++ ")"
    BuilderPartWaitLink linkName _ _ ->
      putStrLn $ "  console.warn('wait no draw: ' + " ++ show linkName ++ ")"
  putStrLn   "})();"

