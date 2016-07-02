module Main where

import HVG.SVG
import HVG.SVGState
import HVG.Type

import Graph

main :: IO ()
main = drawSVG 500 500 graph
