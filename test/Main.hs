module Main where

import HVG
import HVG.Type
import HVG.ContextState

import Graph

main :: IO ()
main = do
  putStrLn "<!Doctype html>"
  putStrLn "<canvas width=1200 height=800></canvas>"
  putStrLn "<script>"
  drawCanvas "canvas" (Size 1200 800) graph
  putStrLn "</script>"
