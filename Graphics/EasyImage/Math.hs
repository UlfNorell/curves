{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-| Simple two-dimensional linear algebra.
 -}
module Graphics.EasyImage.Math
  (
    -- * Vectors
    Scalar, Vec(..), Point
  , unitX, unitY, diag
  , vmap, vzip, vuncurry, vcurry
  , dot, rot90
  , norm, angle
  , interpolate
    -- * Line segments
  , Segment(..)
  , segmentLength, squareSegmentLength
  , leftOf, intersectSegment
    -- * Basis
  , Basis(..), defaultBasis
    -- * Distances
  , DistanceToPoint(..)
    -- * Transformations
  , Transformable(..)
  , translate, scale, scaleFrom, rotate, rotateAround
  ) where

import Control.Applicative
import Test.QuickCheck

type Scalar = Double

-- | Two-dimensional vectors.
data Vec = Vec { getX, getY :: !Scalar }
  deriving (Eq, Ord)

type Point = Vec

instance Show Vec where
  show (Vec x y) = "(" ++ show x ++ " " ++ show y ++ ")"

-- | @unitX = Vec 1 0@
unitX :: Vec
unitX = Vec 1 0

-- | @unitY = Vec 0 1@
unitY :: Vec
unitY = Vec 0 1

-- | @diag x = Vec x x@
diag :: Scalar -> Vec
diag x = Vec x x

-- | Apply a function to the coordinates of a vector.
vmap :: (Scalar -> Scalar) -> Vec -> Vec
vmap f (Vec x y) = Vec (f x) (f y)

-- | Point-wise lifting of an operator on coordinates.
vzip :: (Scalar -> Scalar -> Scalar) -> Vec -> Vec -> Vec
vzip f (Vec x1 y1) (Vec x2 y2) = Vec (f x1 x2) (f y1 y2)

-- | @vuncurry f (Vec x y) = f x y@
vuncurry :: (Scalar -> Scalar -> a) -> Vec -> a
vuncurry f (Vec x y) = f x y

-- | @vcurry f x y = f (Vec x y)@
vcurry :: (Vec -> a) -> Scalar -> Scalar -> a
vcurry f x y = f (Vec x y)

-- | Numbers are lifted to vectors using 'diag'. Arithmetic operations apply
--   point-wise.
instance Num Vec where
  (+)              = vzip (+)
  (-)              = vzip (-)
  (*)              = vzip (*)
  negate           = vmap negate
  abs v            = diag (distance v 0)
  signum (Vec a b) = diag (atan2 b a)
  fromInteger n    = diag (fromInteger n)

instance Fractional Vec where
  (/)          = vzip (/)
  recip        = vmap recip
  fromRational = diag . fromRational

-- | The dot product of two vectors.
dot :: Vec -> Vec -> Scalar
dot (Vec x1 y1) (Vec x2 y2) = x1 * x2 + y1 * y2

-- | Rotate a vector 90 degrees counterclockwise.
rot90 :: Vec -> Vec
rot90 (Vec x y) = Vec (-y) x

-- | Normalize a vector.
--
-- > norm v = v / abs v
norm :: Vec -> Vec
norm v = v / abs v

-- | The counterclockwise angle between two vectors.
angle :: Vec -> Vec -> Scalar
angle (Vec x1 y1) (Vec x2 y2)
  | α > β     = 2 * pi + β - α
  | otherwise = β - α
  where
    α = atan2 y1 x1
    β = atan2 y2 x2

-- | The weighted average of two points.
--
-- > interpolate p q 0 == p
-- > interpolate p q 1 == q
interpolate :: Point -> Point -> Scalar -> Point
interpolate p q t = p + diag t * (q - p)

-- Segments ---------------------------------------------------------------

data Segment = Seg { getStart, getEnd :: !Point }
  deriving (Eq, Ord)

instance Show Segment where
  show (Seg p0 p1) = concat [show p0, "-", show p1]

-- | The square length of a segment. Avoids computing a square root.
squareSegmentLength :: Segment -> Scalar
squareSegmentLength (Seg p0 p1) = squareDistance p0 p1

-- | The length of a segment.
--
-- > segmentLength (Seg p q) = distance p q
segmentLength :: Segment -> Scalar
segmentLength (Seg p0 p1) = distance p0 p1

squareDistanceToSegment :: Segment -> Point -> Scalar
squareDistanceToSegment seg@(Seg p0 p1) p
  | t <= 0  = d0
  | t >= 1  = d1
  | otherwise = max 0 $ d0 - t^2 * s
  where
    s  = squareDistance p0 p1
    d0 = squareDistance p p0
    d1 = squareDistance p p1
    t  = projectOnSegment seg p

projectOnSegment :: Segment -> Point -> Scalar
projectOnSegment (Seg p0 p1) _ | p0 == p1 = 0
projectOnSegment s@(Seg (Vec x0 y0) (Vec x1 y1)) (Vec x y) = (f x0 x1 x + f y0 y1 y) / squareSegmentLength s
  where
    f x0 x1 x = x0 * (x0 - x - x1) + x * x1

-- | Compute the intersection point of two segments, if any.
intersectSegment :: Segment -> Segment -> Maybe Point
intersectSegment l1@(Seg p0 p1) l2@(Seg q0 q1)
  | leftOf p0 l2 == leftOf p1 l2 = Nothing
  | leftOf q0 l1 == leftOf q1 l1 = Nothing
  | otherwise = Just $ Vec x y
  where
    p = p1 - p0
    q = q1 - q0
    u # v = dot u (rot90 v)

    x = (getX q * (p # p0) - getX p * (q # q0)) / (p # q)
    y = (getY q * (p # p0) - getY p * (q # q0)) / (p # q)

-- | Is a point to the left of a line segment, as seen from the start of the
--   segment looking a the end?
leftOf :: Point -> Segment -> Bool
leftOf p (Seg p0 p1) = dot (rot90 $ p - p0) (p1 - p0) < 0

-- Distances --------------------------------------------------------------

class DistanceToPoint a where
  -- | Compute the distance from an @a@ to a given point. Default implementation:
  --
  -- > distance x p = sqrt (squareDistance x p)
  distance       :: a -> Point -> Scalar

  -- | The square of the distance from an @a@ to a point. Default implementation:
  --
  -- > squareDistance x p = distance x p ^ 2
  squareDistance :: a -> Point -> Scalar

  -- | The distance from an @a@ to a point if it's less than a given value.
  --   @distanceAtMost d x p == Nothing@ iff @distance x p > d@.
  distanceAtMost :: Scalar -> a -> Point -> Maybe Scalar

  distance x p       = sqrt (squareDistance x p)
  squareDistance x p = distance x p ^ 2

  distanceAtMost d x p = case distance x p of
    d' | d' <= d   -> Just d'
       | otherwise -> Nothing

instance DistanceToPoint Vec where
  squareDistance (Vec x0 y0) (Vec x1 y1) = (x1 - x0)^2 + (y1 - y0)^2

instance DistanceToPoint Segment where
  squareDistance = squareDistanceToSegment

-- Transformations --------------------------------------------------------

class Transformable a where
  -- | Apply a transformation to all points in an object.
  transform :: (Point -> Point) -> a -> a

instance Transformable Vec where
  transform f = f

instance Transformable Segment where
  transform f (Seg p q) = Seg (f p) (f q)

instance Transformable a => Transformable (Maybe a) where
  transform f = fmap (transform f)

instance Transformable a => Transformable [a] where
  transform f = map (transform f)

instance Transformable () where
  transform _ x = x

instance Transformable b => Transformable (a -> b) where
  transform f g = transform f . g

instance (Transformable a, Transformable b) => Transformable (a, b) where
  transform f (x, y) = (transform f x, transform f y)

instance (Transformable a, Transformable b, Transformable c) => Transformable (a, b, c) where
  transform f (x, y, z) = (transform f x, transform f y, transform f z)

-- | > translate v = transform (+ v)
translate :: Transformable a => Vec -> a -> a
translate v = transform (v +)

-- | > scale v = transform (* v)
scale :: Transformable a => Vec -> a -> a
scale v = transform (* v)

-- | Scale using a given point as the center.
scaleFrom :: Transformable a => Point -> Vec -> a -> a
scaleFrom p v = translate p . scale v . translate (-p)

-- | Rotate an object counterclockwise around the origin.
rotate :: Transformable a => Scalar -> a -> a
rotate α = transform (rot α)
  where
    rot α (Vec x y) = Vec (x * cs - y * sn) (x * sn + y * cs)
      where
        cs = cos α
        sn = sin α

-- | Rotate an object counterclockwise around a given point.
rotateAround :: Transformable a => Point -> Scalar -> a -> a
rotateAround p α = translate p . rotate α . translate (-p)

-- Basis ------------------------------------------------------------------

-- | A basis for a coordinate system.
data Basis = Basis { origin, xUnit, yUnit :: Point }
  deriving (Show, Eq, Ord)

-- | > defaultBasis = Basis 0 unitX unitY
defaultBasis :: Basis
defaultBasis = Basis 0 unitX unitY

instance Transformable Basis where
  transform f (Basis o x y) = Basis (transform f o) (transform f x) (transform f y)

