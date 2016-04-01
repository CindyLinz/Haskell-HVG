import Data.Monoid
import Control.Monad

import HVG
import HVG.Type
import HVG.Context2D
import HVG.ContextState

main = drawCanvas "canvas" (Size 1000 800) $ do
  box 450 10 100 590 1 $ pure $ do
    textTop "world"
    text "aaa"

    name "a"
    ellipse 10 50 80 40 $ do
      text "---"

    link "a" "b" "red"

  g 150 50 $ do
    name "b"
    box 10 100 180 80 1 $ pure $ do
      text "Hi"

    name "c"
    box 120 240 160 80 1 $ pure $ do
      text "yes"

    name "d"
    box (-70) 290 160 80 1 $ pure $ do
      text "no"

    curveLink "b" "c"
    curveLink "b" "d"


  forM_ (zip [1..] (take 10 sepSeries)) $ \(i, d) -> do
    ellipse (d * 800 + 10) 700 30 30 $ do
      text (show i)

{-
textWithBorder :: String -> Builder ()
textWithBorder str = local $ do
  addDraw $ do
    width <- measureText str "width"
      width = 30
    strokeRect (Point 0 0) (Size (width + 10) 20)

    putStrLn "ctx.strokeRect(0, 0, 40, 20);"
    putStrLn "ctx.strokeRect(0, 0, width+40, 20);"
-}

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

g :: Double -> Double -> Builder () -> Builder ()
g x y body = local $ do
  applyTransform (translateMatrix x y)
  body

box :: Double -> Double -> Double -> Double -> Int -> [Builder ()] -> Builder ()
box x y w h level bodies = local $ do
  applyTransform (translateMatrix x y)
  setSize (Size w h)
  tran <- getTransform

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
  addLink link

  forM_ (zip [1..] bodies) $ \(i, body) -> do
    setSize (Size w (h / fromIntegral level))
    body
    applyTransform (translateMatrix 0 (h / fromIntegral level))

ellipse :: Double -> Double -> Double -> Double -> Builder () -> Builder ()
ellipse x y w h body = local $ do
  applyTransform (translateMatrix x y)
  tran <- getTransform
  let centerTran = tran <> translateMatrix (w/2) (h/2)

  setSize (Size w h)

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
  addLink link

  body

textTop :: String -> Builder ()
textTop str = local $ do
  tran <- getTransform
  Size w h <- getSize

  addDraw $ do
    transform tran
    textBaseline TextMiddle
    textAlign TextCenter

    font "15px sans-serif"
    fillStyle "#000"
    fillText str (Point (w / 2) 15) Nothing

text :: String -> Builder ()
text str = local $ do
  tran <- getTransform
  Size w h <- getSize

  addDraw $ do
    transform tran
    textBaseline TextMiddle
    textAlign TextCenter

    font "15px sans-serif"
    fillStyle "#000"
    fillText str (Point (w / 2) (h / 2)) Nothing

link :: String -> String -> String -> Builder ()
link aName bName color = fork $ do
  aLink <- queryLink aName
  bLink <- queryLink bName
  let
    bestLinkPair n aLink bLink =
      fst $ foldl
        (\(best, bestDis) (cha, chaDis) -> if bestDis <= chaDis then (best, bestDis) else (cha, chaDis))
        ((Point 0 0, Point 0 0), 1/0)
        [((a, b), aCost + pointDistance a b + bCost) | LinkPoint a aCost <- take n aLink, LinkPoint b bCost <- take n bLink]

    (aEnd, bEnd) = bestLinkPair 64 aLink bLink


  addDraw $ do
    transform identityMatrix
    strokeStyle color -- "#000"

    lineWidth 1

    beginPath
    moveTo aEnd
    lineTo bEnd
    stroke

  addLink [LinkPoint (interpolatePoint aEnd bEnd 0.5) 0]

curveLink :: String -> String -> Builder ()
curveLink aName bName = fork $ do
  aLink <- queryLink aName
  bLink <- queryLink bName
  let
    bestLinkPair n aLink bLink =
      fst $ foldl
        (\(best, bestDis) (cha, chaDis) ->
          if bestDis <= chaDis then
            (best, bestDis)
          else
            (cha, chaDis))
        ((Point 0 0, Point 0 0), 1/0)
        [ ((a, b), aCost + pointDistance a b + bCost)
        | LinkPoint a aCost <- take n aLink
        , LinkPoint b bCost <- take n bLink]

    (aEnd, bEnd) = bestLinkPair 64 aLink bLink

    Point x1 y1 = aEnd
    Point x2 y2 = bEnd


  addDraw $ do
    transform identityMatrix
    strokeStyle "#000"

    lineWidth 1

    beginPath

    moveTo aEnd
    if abs (x1 - x2) < 10 || abs (y1 - y2) < 10 then
      lineTo bEnd
    else if abs (x1 - x2) < abs (y1 - y2) then
      bezierCurveTo
        (Point x1 (y1 + 40 * (y2 - y1) / abs (y2 - y1)))
        (Point x2 (y2 - 40 * (y2 - y1) / abs (y2 - y1)))
        bEnd
    else
      bezierCurveTo
        (Point (x1 + 40 * (x2 - x1) / abs (x2 - x1)) y1)
        (Point (x2 - 40 * (x2 - x1) / abs (x2 - x1)) y2)
        bEnd

    stroke

  addLink [LinkPoint (interpolatePoint aEnd bEnd 0.5) 0]

