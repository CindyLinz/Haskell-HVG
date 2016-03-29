module Main where

import HVG
import HVG.Type

import Graph

main :: IO ()
main = do
  putStrLn "<!Doctype html>"
  putStrLn "<canvas width=800 height=800></canvas>"
  putStrLn "<script>"
  drawCanvas "canvas" (Size 800 800) graph
  putStrLn "</script>"
