{-# LANGUAGE MultiWayIf #-}
module Graphics.EasyImage.Geometry where

import Graphics.EasyImage.Math
import Graphics.EasyImage.Image
import Graphics.EasyImage.Curve
import Graphics.EasyImage.Text
import Graphics.EasyImage.Attribute

rectangle :: Point -> Point -> Image
rectangle p q = poly [p, Vec (getX q) (getY p), q, Vec (getX p) (getY q)]

type Length = Scalar
type Angle  = Scalar

triangle :: Length -> Length -> Length -> Image
triangle a b c
  | a + b < c ||
    b + c < a ||
    c + a < b = error $ unwords ["not a triangle:", show a, show b, show c]
  | otherwise = triangleA a b $ acos $ (a^2 + b^2 - c^2) / (2 * a * b)

triangleA :: Length -> Length -> Angle -> Image
triangleA a b α = poly [0, Vec a 0, rotate α $ Vec b 0]

triangleAA :: Length -> Angle -> Angle -> Image
triangleAA a α β = triangleA a (a * sin β / sin (α + β)) α

regularPoly :: Int -> Image
regularPoly n | n < 3 = error "regularPoly: n < 3"
regularPoly n = poly [ rotate (2 * pi * fromIntegral i / fromIntegral n) unitX | i <- [0..n - 1] ]

angleArc :: Point -> Point -> Point -> Image
angleArc p0 p1 p2 = curve' f g 0 1
  where
    f _ = (p0, p1, p2)
    g t (p0, p1, p2) = p0 + 20 * rot (t * α) (norm (p1 - p0))
      where
        α = angle (p1 - p0) (p2 - p0)

labelledAngle s d p0 p1 p2 =
  angleArc p0 p1 p2
  <> freezeImageSize p0 (label (p0 + d * norm (v1 + v2)) 14 s)
  where
    v1 = norm $ p1 - p0
    v2 = norm $ p2 - p0

arrow :: Point -> Point -> Image
arrow from to = curve' f g 0 2 <> line from to
  where
    f = const $ Seg from to
    g t (Seg from to) =
        if | t <= 1    -> interpolate fin1 to t
           | otherwise -> interpolate to fin2 (t - 1)
      where
        v    = norm (from - to)
        fin1 = to + 20 * rot (pi/6) v
        fin2 = to + 20 * rot (-pi/6) v

