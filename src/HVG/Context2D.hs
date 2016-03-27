module HVG.Context2D where

import HVG.Type

-------------------------------
-- Canvas context 2d command
--

save :: IO ()
save = putStrLn "ctx.save();"

restore :: IO ()
restore = putStrLn "ctx.restore();"

lineWidth :: Double -> IO ()
lineWidth val = putStrLn $ "ctx.lineWidth = " ++ show val ++ ";"

lineCap :: LineCap -> IO ()
lineCap val = putStrLn $ "ctx.lineCap = " ++ show (strValue val) ++ ";"

lineJoin :: LineJoin -> IO ()
lineJoin val = putStrLn $ "ctx.lineJoin = " ++ show (strValue val) ++ ";"

miterLimit :: Double -> IO ()
miterLimit val = putStrLn $ "ctx.miterLimit = " ++ show val ++ ";"

lineDash :: [Double] -> IO ()
lineDash val = putStrLn $ "ctx.setLineDash(" ++ show val ++ ");"

lineDashOffset :: Double -> IO ()
lineDashOffset val = putStrLn $ "ctx.lineDashOffset = " ++ show val ++ ";"

font :: String -> IO ()
font val = putStrLn $ "ctx.font = " ++ show val ++ ";"

textAlign :: TextAlign -> IO ()
textAlign val = putStrLn $ "ctx.textAlign = " ++ show (strValue val) ++ ";"

textBaseline :: TextBaseline -> IO ()
textBaseline val = putStrLn $ "ctx.textBaseline = " ++ show (strValue val) ++ ";"


moveTo :: Point -> IO ()
moveTo (Point x y) = putStrLn $ "ctx.moveTo(" ++ show x ++ "," ++ show y ++ ");"

closePath :: IO ()
closePath = putStrLn $ "ctx.closePath();"

lineTo :: Point -> IO ()
lineTo (Point x y) = putStrLn $ "ctx.lineTo(" ++ show x ++ "," ++ show y ++ ");"

quadraticCurveTo :: Point -> Point -> IO ()
quadraticCurveTo (Point cx cy) (Point x y) =
  putStrLn $ "ctx.quadraticCurveTo(" ++ show cx ++ "," ++ show cy ++ "," ++ show x ++ "," ++ show y ++ ");"

bezierCurveTo :: Point -> Point -> Point -> IO ()
bezierCurveTo (Point cx1 cy1) (Point cx2 cy2) (Point x y) =
  putStrLn $ "ctx.bezierCurveTo(" ++ show cx1 ++ "," ++ show cy1 ++ "," ++ show cx2 ++ "," ++ show cy2 ++ "," ++ show x ++ "," ++ show y ++ ");"

arcTo :: Point -> Point -> Double -> IO ()
arcTo (Point x1 y1) (Point x2 y2) r =
  putStrLn $ "ctx.arcTo(" ++ show x1 ++ "," ++ show y1 ++ "," ++ show x2 ++ "," ++ show y2 ++ "," ++ show r ++ ");"

arc :: Point -> Double -> Double -> Double -> RotateDirection -> IO ()
arc (Point x y) r start end dir =
  putStrLn $ "ctx.arc(" ++ show x ++ "," ++ show y ++ "," ++ show r ++ "," ++ show start ++ "," ++ show end ++ "," ++ (case dir of { CW -> "false" ; CCW -> "true" }) ++ ");"

rect :: Point -> Size -> IO ()
rect (Point x y) (Size w h) = putStrLn $ "ctx.rect(" ++ show x ++ "," ++ show y ++ "," ++ show w ++ "," ++ show h ++ ");"

transform :: Matrix -> IO ()
transform
  ( Matrix
    a11 a12 a13
    a21 a22 a23
  )
  = putStrLn $ "ctx.setTransform(" ++ show a11 ++ "," ++ show a21 ++ "," ++ show a12 ++ "," ++ show a22 ++ "," ++ show a13 ++ "," ++ show a23 ++ ");"


fillStyle :: String -> IO ()
fillStyle val = putStrLn $ "ctx.fillStyle = " ++ show val ++ ";"

strokeStyle :: String -> IO ()
strokeStyle val = putStrLn $ "ctx.strokeStyle = " ++ show val ++ ";"


clearRect :: Point -> Size -> IO ()
clearRect (Point x y) (Size w h) =
  putStrLn $ "ctx.clearRect(" ++ show x ++ "," ++ show y ++ "," ++ show w ++ "," ++ show h ++ ");"

fillRect :: Point -> Size -> IO ()
fillRect (Point x y) (Size w h) =
  putStrLn $ "ctx.fillRect(" ++ show x ++ "," ++ show y ++ "," ++ show w ++ "," ++ show h ++ ");"

strokeRect :: Point -> Size -> IO ()
strokeRect (Point x y) (Size w h) =
  putStrLn $ "ctx.strokeRect(" ++ show x ++ "," ++ show y ++ "," ++ show w ++ "," ++ show h ++ ");"


fillText :: String -> Point -> Maybe Double -> IO ()
fillText str (Point x y) = \case
  Nothing -> putStrLn $ "ctx.fillText(" ++ show str ++ "," ++ show x ++ "," ++ show y ++ ");"
  Just w -> putStrLn $ "ctx.fillText(" ++ show str ++ "," ++ show x ++ "," ++ show y ++ "," ++ show w ++ ");"

strokeText :: String -> Point -> Maybe Double -> IO ()
strokeText str (Point x y) = \case
  Nothing -> putStrLn $ "ctx.strokeText(" ++ show str ++ "," ++ show x ++ "," ++ show y ++ ");"
  Just w -> putStrLn $ "ctx.strokeText(" ++ show str ++ "," ++ show x ++ "," ++ show y ++ "," ++ show w ++ ");"

measureText :: String -> IO Double
measureText str = error "Can't mesureText when offline"


beginPath :: IO ()
beginPath = putStrLn "ctx.beginPath();"

fill :: IO ()
fill = putStrLn "ctx.fill();"

stroke :: IO ()
stroke = putStrLn "ctx.stroke();"

clip :: IO ()
clip = putStrLn "ctx.clip();"

isPointInPath :: Point -> IO Bool
isPointInPath (Point x y) = error "Can't check inPointInPath"


globalAlpha :: Double -> IO ()
globalAlpha val = putStrLn $ "ctx.globalAlpha = " ++ show val ++ ";"

globalCompositeOperation :: CompositeOp -> IO ()
globalCompositeOperation val = putStrLn $ "ctx.globalCompositeOperation = " ++ show (strValue val) ++ ";"


shadowColor :: String -> IO ()
shadowColor val = putStrLn $ "ctx.shadowColor = " ++ show val ++ ";"

shadowOffsetX :: Double -> IO ()
shadowOffsetX val = putStrLn $ "ctx.shadowOffsetX = " ++ show val ++ ";"

shadowOffsetY :: Double -> IO ()
shadowOffsetY val = putStrLn $ "ctx.shadowOffsetY = " ++ show val ++ ";"

shadowBlur :: Double -> IO ()
shadowBlur val = putStrLn $ "ctx.shadowBlur = " ++ show val ++ ";"
