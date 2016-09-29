module HVG.SVG where

import qualified Data.Map.Strict as M
import Control.Monad

import HVG.Type
import HVG.SVGState

drawSVG :: Double -> Double -> Builder info SVGState SVGDrawing () -> IO ()
drawSVG width height drawBuilder = do
  let (cmds, pendings) = execBuilder drawBuilder initSVGState []
  putStrLn $ svgCommand cmds
  putStrLn $ "<!-- pending names: " ++ show pendings ++ " -->"
  {-
  case drawBuilder Nothing initSVGState initBuilderState of
    BuilderPartDone _ _ bld _ -> do
      svg width height $ do
        putStrLn $ svgCommand (bldDraw bld)
      forM_ (M.toList (bldWaitInfo bld)) $ \(infoName, _) ->
        putStrLn $ "<!-- wait no info: " ++ show infoName ++ " -->"
    BuilderPartWaitInfo infoName _ _ ->
      putStrLn $ "<!-- wait no draw: " ++ show infoName ++ " -->"
  -}

svgHeader :: Double -> Double -> String
svgHeader w h = "\
  \<?xml version=\"1.0\" standalone=\"no\"?>\n\
  \<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \n\
  \  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\
  \<svg width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\" version=\"1.1\" viewBox=\"0 0 " ++ show w ++ " " ++ show h ++ "\"\n\
  \     xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink= \"http://www.w3.org/1999/xlink\">\n"

svgFooter :: String
svgFooter = "\n</svg>"

svg :: Double -> Double -> IO () -> IO ()
svg width height content = do
  putStrLn $ svgHeader width height
  content
  putStrLn $ svgFooter

