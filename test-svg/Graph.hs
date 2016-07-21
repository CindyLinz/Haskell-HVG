module Graph where

import Data.Traversable
import Control.Monad

import HVG.Type
import HVG.SVG
import HVG.SVGState

import Entity

graph :: Builder () SVGState SVGDrawing ()
graph = do
  tr 39 1 $ do
    contour $ do
      let
        halfC = do
          l ((-67/2) + 2) 0
          a (-2) 2 90
          l (-3.1) 0
          a (-1) (-1) (-90)
          l (-1) 0
          l 0 89
          a 5 5 90
          l (67/2+3.1+2 - 3 - 5) 0
          sc 1 (10/3) $ do
            a 3 3 90
      p 0 0 halfC
      rp 0 0 $ sc (-1) 1 $ halfC

    contour $ do
      let
        halfC = do
          l (6/2 + 18 - 5) 0
          a 5 (-5) 90
          l 0 (-70)
          a (-5) (-5) 90
          l (-(18 - 5)) 0
          sc 1 (10/3) $ do
            a (-3) 3 (-90)

      tr 0 85 $ do
        p 0 0 halfC
        rp 0 0 $ sc (-1) 1 $ halfC

    let
      screwHole =
        contour $ p 0 (-2.1) $ do
          a 0 4.2 180
          a 0 (-4.2) 180

    tr (-56/2) 75 screwHole
    tr (56/2) 75 screwHole
    tr (-56/2) 55 screwHole
    tr (56/2) 55 screwHole
    tr (-56/2) 35 screwHole
    tr (56/2) 35 screwHole

    tr (-46/2) 90 screwHole
    tr (46/2) 90 screwHole

  let
    sidePanel = do
      contour $ do
        p 0 0 $ do
          l 0 (-3)
          a 2 (-2) (-90)
          l (43.8 - 2 - 2) 0
          a 2 2 (-90)
          l 0 3
          l (10.1 - 2) 0
          a 2 2 (-90)
          --l (-(64 - 5)) (111 - 5 - 2)
          b 0 15 (-(64 - 5) + 15) (111 - 5 - 2) (-(64 - 5)) (111 - 5 - 2)
          a (-5) (-5) (-90)
          l 0 (-(111 - 5 - 5 - 2))
          a 2 (-2) (-90)
          l (3.1 + 5) 0

      contour $ do
        p (-(3.1 + 5)) 5 $ do
          l 3.1 0
          l 0 96
          l (-3.1) 0
          l 0 (-96)

      contour $ do
        p 5 5 $ do
          l 10 0
          a 3 3 (-90)
          --l (-23) 82
          b 0 5 (-16.5+3) 82 (-16.5) 82
          a (-1.5) (-1.5) (-90)
          l 0 (-78.5)
          a 5 (-5) (-90)

      contour $ do
        p 43 5 $ do
          a 3 3 (-90)
          --l (-30) 60
          b 0 5 (-32+5) 65 (-32) 65
          a (-1) (-1) (-90)
          l 10 (-62)
          a 5 (-5) (-90)

  tr 60 6 $ do
    tr 29 0 sidePanel
    tr 73 155 (sc (-1) (-1) sidePanel)

  tr 39 107 $ do
    --rt 90 $ do
      contour $ do
        p 0 0 $ do
          l (77.2/2 - 2) 0
          a 2 2 (-90)
          l 0 (64 -2 -2)
          a (-2) 2 (-90)
          l (-(77.2/2 - 2) * 2) 0
          a (-2) (-2) (-90)
          l 0 (-(64 - 2 - 2))
          a 2 (-2) (-90)
          l (77.2/2 - 2) 0

      let
        sideHole = contour $ do
          p 0 0 $ do
            l (3.1/2) 0
            l 0 44
            l (-3.1) 0
            l 0 (-44)
            l (3.1/2) 0

        wireHole = contour $ do
          p 0 (-(2.5/2)) $ do
            a 0 2.5 180
            a 0 (-2.5) 180

        knockerHole = contour $ do
          p 0 0 $ do
            let r = 3.1 / 2
            a r r (-90)
            l 0 (10 - r * 2)
            a (-r*2) 0 (-180)
            l 0 (-(10 - r * 2))
            a r (-r) (-90)

      tr (67/2 + 3.1/2) 10 sideHole
      tr (-(67/2 + 3.1/2)) 10 sideHole

      tr (11/2) 22 knockerHole
      tr (-(11/2)) 22 knockerHole

      tr (11/2) (22+10+22) wireHole
      tr (-(11/2)) (22+10+22) wireHole

