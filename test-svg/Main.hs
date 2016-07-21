module Main where

import HVG.SVG
import HVG.SVGState
import HVG.Type

import Graph

main :: IO ()
main = drawSVG 145 172 graph
