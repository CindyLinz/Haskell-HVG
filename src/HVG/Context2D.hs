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
