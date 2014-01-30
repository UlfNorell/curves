{-# LANGUAGE MultiWayIf #-}
{-| Combinators for drawing various geometric figures.
 -}
module Graphics.Curves.Geometry where

import Graphics.Curves.Math
import Graphics.Curves.Image
import Graphics.Curves.Curve
import Graphics.Curves.Attribute
import Graphics.Curves.Text

-- | Draw an axis-aligned rectangle with the given opposite corners.
rectangle :: Point -> Point -> Image
rectangle p q = poly [p, Vec (getX q) (getY p), q, Vec (getX p) (getY q)]

type Length = Scalar
type Angle  = Scalar

-- | Draw a triangle with the given side lengths. The corner opposite the third
--   side is located at the origin and the first side is drawn along the
--   x-axis.
triangle :: Length -> Length -> Length -> Image
triangle a b c
  | a + b < c ||
    b + c < a ||
    c + a < b = error $ unwords ["not a triangle:", show a, show b, show c]
  | otherwise = triangleA a b $ acos $ (a^2 + b^2 - c^2) / (2 * a * b)

-- | Draw a triangle given by two side lengths and an angle. The corner of the
--   angle is drawn at the origin and the first side along the x-axis.
triangleA :: Length -> Length -> Angle -> Image
triangleA a b α = poly [0, Vec a 0, rotate α $ Vec b 0]

-- | Draw a triangle given by two angles and the length of the side shared by
--   the angles. The first angle is drawn at the origin and the side along the
--   x-axis.
triangleAA :: Length -> Angle -> Angle -> Image
triangleAA a α β = triangleA a (a * sin β / sin (α + β)) α

-- | Draw an n-sided regular polygon centered at the origin and one corner at
--   (1, 0).
regularPoly :: Int -> Image
regularPoly n | n < 3 = error "regularPoly: n < 3"
regularPoly n = poly [ rotate (2 * pi * fromIntegral i / fromIntegral n) unitX | i <- [0..n - 1] ]

-- | Draw an angle arc for the counter clockwise angle BAC.
angleArc :: Scalar -- ^ Radius of the arc in pixels
         -> Point  -- ^ A
         -> Point  -- ^ B
         -> Point  -- ^ C
         -> Image
angleArc w p0 p1 p2 = curve' 0 1 f g
  where
    f _ = (p0, p1, p2)
    g t (p0, p1, p2) = p0 + diag w * rotate (t * α) (norm (p1 - p0))
      where
        α = angle (p1 - p0) (p2 - p0)

-- | Draw an angle arc labelled by the given string.
labelledAngle :: String -> Vec -- ^ Position of the text relative to the angle
              -> Point -> Point -> Point -> Image
labelledAngle s d p0 p1 p2 =
  angleArc 20 p0 p1 p2
  <> freezeImageSize p0 (label (p0 + d * norm (v1 + v2)) 14 s)
  where
    v1 = norm $ p1 - p0
    v2 = norm $ p2 - p0

-- | Draw a line segment with an arrow head.
arrow :: Point -> Point -> Image
arrow from to = curve' 0 2 f g <> line from to
  where
    f = const $ Seg from to
    g t (Seg from to) =
        if | t <= 1    -> interpolate fin1 to t
           | otherwise -> interpolate to fin2 (t - 1)
      where
        v    = norm (from - to)
        fin1 = to + 20 * rotate (pi/6) v
        fin2 = to + 20 * rotate (-pi/6) v

