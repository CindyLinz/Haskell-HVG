module HVG.Geometry where

import Data.Monoid

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

newtype Size = SizeVec Vec
pattern Size x y = SizeVec (Vec x y)

newtype Dis = DisVec Vec
pattern Dis x y = DisVec (Vec x y)

newtype Angle = Angle Double deriving (Num, Fractional, Real, RealFrac, Floating, RealFloat, Eq, Ord)
pattern Rad rad = Angle rad
pattern Deg deg <- ((\(Rad rad) -> rad / pi * 180) -> deg) where
  Deg deg = Rad (deg / 180 * pi)

data Trans = Trans
  {-# UNPACK #-} !Mat -- trans matrix
  {-# UNPACK #-} !Mat -- inverted trans matrix

instance Monoid Trans where
  mempty = Trans mempty mempty
  mappend (Trans a ai) (Trans b bi) =
    Trans
      (a <> b)
      (ai <> bi)

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

transitionPos :: Double -> Double -> Trans
transitionPos x y = Trans
  ( Mat
    1 0 x
    0 1 y
  )
  ( Mat
    1 0 (0 - x)
    0 1 (0 - y)
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
      c (0 - s) 0
      s    c    0
    )
    ( Mat
         c    s 0
      (0 - s) c 0
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

data ConicSectionCoef = ConicSectionCoef
  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
data ConicSection = ConicSectionData
  ConicSectionCoef
  ConicSectionParam

data ConicSectionParam
  = CircleParam
    {-# UNPACK #-} !Pos -- center
    {-# UNPACK #-} !Double -- radius
  | EllipseParam
    {-# UNPACK #-} !Pos -- center
    {-# UNPACK #-} !Size -- major radius, minor radius
    {-# UNPACK #-} !Angle -- 0 means x-axis as major axis
  | HyperbolaParam
    {-# UNPACK #-} !Pos -- center
    {-# UNPACK #-} !Size -- major radius, minor radius
    {-# UNPACK #-} !Angle -- 0 means x-axis as major axis
  | ParabolaParam
    {-# UNPACK #-} !Pos -- vertex
    {-# UNPACK #-} !Double -- focal length
    {-# UNPACK #-} !Angle -- 0 means x-axis positive direction as open direction

coerceEllipseParam :: ConicSectionParam -> Maybe ConicSectionParam
coerceEllipseParam (CircleParam center r) = Just $ EllipseParam center (Size r r) (Angle 0)
coerceEllipseParam ell@(EllipseParam _ _ _) = Just ell
coerceEllipseParam _ = Nothing

pattern Circle center r <- ConicSectionData _ (CircleParam center r) where
  Circle ~center@(Pos cx cy) r = ConicSectionData
    (ConicSectionCoef 1 0 1 (0 - 2*cx) (0 - 2*cy) (cx*cx + cy*cx - r*r))
    (CircleParam center r)

pattern Ellipse center rs angle <- ConicSectionData _ (coerceEllipseParam -> Just (EllipseParam center rs angle)) where
  Ellipse ~center@(Pos cx cy) ~rs@(Size rx ry) angle@(Rad rad) = ConicSectionData
    (ConicSectionCoef a b c d e f)
    (if rx == ry then CircleParam center rx else EllipseParam center rs angle)
    where
      a = cosAngleQuotRx*cosAngleQuotRx + sinAngleQuotRy*sinAngleQuotRy
      b = (-2) * cosAngle * sinAngle * (1 / rx*rx - 1 / ry*ry)
      c = sinAngleQuotRx*sinAngleQuotRx + cosAngleQuotRy*cosAngleQuotRy
      d = -(2*a*cx + b*cy)
      e = -(2*c*cy + b*cx)
      f = a*cx*cx + b*cx*cy + c*cy*cy - 1
      cosAngle = cos rad
      sinAngle = sin rad
      cosAngleQuotRx = cosAngle / rx
      sinAngleQuotRx = sinAngle / rx
      cosAngleQuotRy = cosAngle / ry
      sinAngleQuotRy = sinAngle / ry

pattern ConicSection a b c d e f <- ConicSectionData (ConicSectionCoef a b c d e f) _ where
  ConicSection a b c d e f = ConicSectionData (ConicSectionCoef a b c d e f) param where
    rad2 | abs (a - c) < 1e-6 = pi / 2
         | otherwise = atan (b / (c - a))
    rad = rad2 / 2

    acSumQuot2 = (a + c) / 2
    acDiffQuot2CosRad2 = (a - c) / 2 * cos rad2
    bQuot2SinRad2 = b / 2 * sin rad2
    cosRad = cos rad
    sinRad = sin rad

    a' = acSumQuot2 + acDiffQuot2CosRad2 - bQuot2SinRad2
    b' = 0
    c' = acSumQuot2 - acDiffQuot2CosRad2 + bQuot2SinRad2
    d' = d*cosRad - e*sinRad
    e' = d*sinRad + e*cosRad
    f' = f

    cx' = d' / (2 * a')
    cy' = e' / (2 * c')

    rad' | abs a' > abs c' = rad
         | rad + pi / 2

    param
      | a' == c' = CircleParam (rotate (-rad) `appTrans` (Pos (-cx') (-cy'))) 
