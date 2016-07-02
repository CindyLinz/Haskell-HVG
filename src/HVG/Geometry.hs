module HVG.Geometry where

import Data.Monoid
import Text.Printf

data Vec = Vec
  {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double

data Mat = Mat
  {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance Monoid Mat where
  mempty = Mat
    1 0 0
    0 1 0
  mappend
    ( Mat
      a11 a12 a13
      a21 a22 a23
    )
    ( Mat
      b11 b12 b13
      b21 b22 b23
    )
    = Mat
      c11 c12 c13
      c21 c22 c23
      where
        c11 = a11 * b11 + a12 * b21
        c12 = a11 * b12 + a12 * b22
        c13 = a11 * b13 + a12 * b23 + a13
        c21 = a21 * b11 + a22 * b21
        c22 = a21 * b12 + a22 * b22
        c23 = a21 * b13 + a22 * b23 + a23

appMat :: Mat -> Vec -> Vec
appMat
  ( Mat
    m11 m12 m13
    m21 m22 m23
  )
  ( Vec x y )
  = Vec
    ( x * m11 + y * m12 + m13 )
    ( x * m21 + y * m22 + m23 )

newtype Pos = PosVec Vec
pattern Pos x y = PosVec (Vec x y)
instance Show Pos where
  show (Pos x y) = printf "(%f, %f)" x y

newtype Size = SizeVec Vec
pattern Size x y = SizeVec (Vec x y)

newtype Dis = DisVec Vec
pattern Dis x y = DisVec (Vec x y)
instance Show Dis where
  show (Dis x y) = printf "(%f, %f)" x y

newtype Angle = Angle {unRad :: Double} deriving (Num, Fractional, Real, RealFrac, Floating, RealFloat, Eq, Ord)
pattern Rad rad = Angle rad
pattern Deg {unDeg} <- ((\(Rad rad) -> rad / pi * 180) -> unDeg) where
  Deg deg = Rad (deg / 180 * pi)

data Trans = Trans
  {-# UNPACK #-} !Mat -- trans matrix
  {-# UNPACK #-} !Mat -- inverted trans matrix

instance Monoid Trans where
  mempty = Trans mempty mempty
  mappend (Trans a ai) (Trans b bi) =
    Trans
      (a <> b)
      (bi <> ai)

infixr 6 `appTrans`, `appMat`
class TransAppable a where
  appTrans :: Trans -> a -> a
  appInvTrans :: Trans -> a -> a
instance TransAppable Trans where
  appTrans = (<>)
  appInvTrans (Trans mat invMat) = appTrans (Trans invMat mat)
instance TransAppable Pos where
  appTrans (Trans mat _) (PosVec vec) = PosVec $ appMat mat vec
  appInvTrans (Trans _ invMat) (PosVec vec) = PosVec $ appMat invMat vec
instance TransAppable Dis where
  appTrans (Trans mat _) (DisVec vec) = DisVec $ appMat (noTransitionMat mat) vec
  appInvTrans (Trans _ invMat) (DisVec vec) = DisVec $ appMat (noTransitionMat invMat) vec

identityTrans :: Trans
identityTrans = Trans identityMat identityMat where
  identityMat = Mat
    1 0 0
    0 1 0

transitionPos :: Double -> Double -> Trans
transitionPos x y = Trans
  ( Mat
    1 0 x
    0 1 y
  )
  ( Mat
    1 0 (-x)
    0 1 (-y)
  )

transition :: Pos -> Trans
transition (Pos x y) = transitionPos x y

noTransition :: Trans -> Trans
noTransition (Trans mat invMat) = Trans (noTransitionMat mat) (noTransitionMat invMat)

noTransitionMat :: Mat -> Mat
noTransitionMat
  ( Mat
    a11 a12 a13
    a21 a22 a23
  ) = Mat
    a11 a12 0
    a21 a22 0

rotateRad :: Double -> Trans
rotateRad rad =
  Trans
    ( Mat
      c (-s) 0
      s    c    0
    )
    ( Mat
         c    s 0
      (-s) c 0
    )
  where
    c = cos rad
    s = sin rad

rotateDeg :: Double -> Trans
rotateDeg deg = rotateRad (deg / 180 * pi)

rotate :: Angle -> Trans
rotate (Rad rad) = rotateRad rad

scale :: Double -> Double -> Trans
scale rx ry = Trans
  ( Mat
    rx 0 0
    0 ry 0
  )
  ( Mat
    (1 / rx) 0 0
    0 (1 / ry) 0
  )

vecSize :: Vec -> Double
vecSize (Vec x y) = sqrt ((x*x) + (y*y))

dis :: Pos -> Pos -> Dis
dis (Pos xFrom yFrom) (Pos xTo yTo) = Dis (xTo - xFrom) (yTo - yFrom)

disSize :: Dis -> Double
disSize (DisVec vec) = vecSize vec

scaleDis :: Double -> Dis -> Dis
scaleDis r (Dis x y) = Dis (x*r) (y*r)

unitDis :: Dis -> Dis
unitDis dis = scaleDis (1 / disSize dis) dis

turnDis :: Angle -> Dis -> Dis
turnDis angle = appTrans (rotate angle)

revDis :: Dis -> Dis
revDis (Dis x y) = Dis (-x) (-y)

turnRightDis :: Dis -> Dis
turnRightDis (Dis x y) = Dis y (-x)

turnLeftDis :: Dis -> Dis
turnLeftDis (Dis x y) = Dis (-y) x

distance :: Pos -> Pos -> Double
distance a b = disSize (dis a b)

midPos :: Pos -> Pos -> Pos
midPos (Pos x1 y1) (Pos x2 y2) = Pos ((x1 + x2)/2) ((y1 + y2)/2)

movePos :: Dis -> Pos -> Pos
movePos (Dis dx dy) (Pos x y) = Pos (x + dx) (y + dy)
