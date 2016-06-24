module Entity where

import HVG.Type
import HVG.Context2D
import HVG.ContextState

import Control.Monad
import Data.Monoid

type Draw = IO ()

{- sepSeries
  0
  1/2
  1/4 3/4
  1/8 3/8 5/8 7/8
  1/16 3/16 5/16 7/16 9/16 11/16 13/16 15/16
  ...
-}
sepSeries :: [Double]
sepSeries = 0 : inners where
  inners = 0.5 : mix halfs (map (+ 0.5) halfs)
  halfs = map (/ 2) inners
  mix (a:as) (b:bs) = a : b : mix as bs

box :: Double -> Double -> Double -> Double -> Int -> [Builder Link ContextState Draw ()] -> Builder Link ContextState Draw ()
box x y w h level bodies = local $ do
  applyTransform (translateMatrix x y)
  setSize (Size w h)
  tran <- getTransform

  let
    center = Point (w / 2) (h / 2)
    link = map pt2link $ join $ flip map sepSeries $ \d ->
      [ Point (w * d) 0
      , Point w (h * d)
      , Point (w * (1 - d)) h
      , Point 0 (h * (1 - d))
      ]
    pt2link p =
      LinkPoint
        (movePoint tran p)
        (pointDistance center p)

  addInfo link
  addDraw $ do
    strokeStyle "#000"
    transform tran

    lineWidth 2
    forM_ [1 .. level - 1] $ \i -> do
      beginPath
      moveTo (Point 0 (h / fromIntegral level * fromIntegral i))
      lineTo (Point w (h / fromIntegral level * fromIntegral i))
      stroke

    lineWidth 3
    strokeRect (Point 0 0) (Size w h)

  forM_ (zip [1..] bodies) $ \(i, body) -> do
    setSize (Size w (h / fromIntegral level))
    body
    applyTransform (translateMatrix 0 (h / fromIntegral level))

ellipse :: Double -> Double -> Double -> Double -> Builder Link ContextState Draw () -> Builder Link ContextState Draw ()
ellipse x y w h body = local $ do
  applyTransform (translateMatrix x y)
  tran <- getTransform
  let centerTran = tran <> translateMatrix (w/2) (h/2)

  setSize (Size w h)

  let
    sepToPoint ratio =
      let
        arg = ratio * 2 * 3.14159265358979323846
      in
        movePoint
          centerTran
          ( Point
            (w/2 * cos arg)
            (h/2 * sin arg)
          )
    link = map (\p -> LinkPoint p 0) $ map sepToPoint sepSeries

  addInfo link
  addDraw $ do
    strokeStyle "#000"
    transform centerTran

    lineWidth 3

    beginPath
    moveTo (Point 0 (h/2))
    bezierCurveTo
      (Point (0.552284749*w/2) (h/2))
      (Point (w/2) (0.552284749*h/2))
      (Point (w/2) 0)
    bezierCurveTo
      (Point (w/2) (-0.552284749*h/2))
      (Point (0.552284749*w/2) (-h/2))
      (Point 0 (-h/2))
    bezierCurveTo
      (Point (-0.552284749*w/2) (-h/2))
      (Point (-w/2) (-0.552284749*h/2))
      (Point (-w/2) 0)
    bezierCurveTo
      (Point (-w/2) (0.552284749*h/2))
      (Point (-0.552284749*w/2) (h/2))
      (Point 0 (h/2))
    stroke

  body

textTop :: String -> Builder Link ContextState Draw ()
textTop str = local $ do
  tran <- getTransform
  Size w h <- getSize

  addInfo []
  addDraw $ do
    transform tran
    textBaseline TextMiddle
    textAlign TextCenter

    font "15px sans-serif"
    fillStyle "#000"
    fillText str (Point (w / 2) 15) Nothing

text :: String -> Builder Link ContextState Draw ()
text str = local $ do
  tran <- getTransform
  Size w h <- getSize

  addInfo []
  addDraw $ do
    transform tran
    textBaseline TextMiddle
    textAlign TextCenter

    font "15px sans-serif"
    fillStyle "#000"
    fillText str (Point (w / 2) (h / 2)) Nothing

link :: String -> String -> Builder Link ContextState Draw ()
link aName bName = fork $ do
  aLink <- queryInfo aName
  bLink <- queryInfo bName
  let
    bestLinkPair n aLink bLink =
      fst $ foldl
        (\(best, bestDis) (cha, chaDis) -> if bestDis <= chaDis then (best, bestDis) else (cha, chaDis))
        ((Point 0 0, Point 0 0), 1/0)
        [((a, b), aCost + pointDistance a b + bCost) | LinkPoint a aCost <- take n aLink, LinkPoint b bCost <- take n bLink]

    (aEnd, bEnd) = bestLinkPair 64 aLink bLink


  addInfo [LinkPoint (interpolatePoint aEnd bEnd 0.5) 0]
  addDraw $ do
    transform identityMatrix
    strokeStyle "#000"

    lineWidth 1

    beginPath
    moveTo aEnd
    lineTo bEnd
    stroke
