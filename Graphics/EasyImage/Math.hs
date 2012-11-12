{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.EasyImage.Math
  (
    Scalar, Vec(..), Point
  , unitX, unitY, diag
  , vmap, vzip, vuncurry, vcurry
  , Segment(..)
  , segmentLength, squareSegmentLength
  , intersectSegment
  , leftOf, dot, rot90
  , norm, angle
  , DistanceToPoint(..)
  , Transformable(..)
  , translate, scale, scaleFrom, rotate, rotateAround
  , interpolate
  , Basis(..), defaultBasis
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

unitX :: Vec
unitX = Vec 1 0

unitY :: Vec
unitY = Vec 0 1

diag :: Scalar -> Vec
diag x = Vec x x

vmap :: (Scalar -> Scalar) -> Vec -> Vec
vmap f (Vec x y) = Vec (f x) (f y)

vzip :: (Scalar -> Scalar -> Scalar) -> Vec -> Vec -> Vec
vzip f (Vec x1 y1) (Vec x2 y2) = Vec (f x1 x2) (f y1 y2)

vuncurry :: (Scalar -> Scalar -> a) -> Vec -> a
vuncurry f (Vec x y) = f x y

vcurry :: (Vec -> a) -> Scalar -> Scalar -> a
vcurry f x y = f (Vec x y)

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

data Segment = Seg { getStart, getEnd :: !Point }
  deriving (Eq, Ord)

instance Show Segment where
  show (Seg p0 p1) = concat [show p0, "-", show p1]

squareSegmentLength :: Segment -> Scalar
squareSegmentLength (Seg p0 p1) = squareDistance p0 p1

segmentLength :: Segment -> Scalar
segmentLength (Seg p0 p1) = distance p0 p1

distanceToSegment :: Segment -> Point -> Scalar
distanceToSegment seg@(Seg p0 p1) p
  | t <= 0  = d0
  | t >= 1  = d1
  | otherwise = sqrt (max 0 $ d0^2 - (t * s)^2)
  where
    s  = distance p0 p1
    d0 = distance p p0
    d1 = distance p p1
    t  = projectOnSegment seg p

projectOnSegment :: Segment -> Point -> Scalar
projectOnSegment (Seg p0 p1) _ | p0 == p1 = 0
projectOnSegment s@(Seg (Vec x0 y0) (Vec x1 y1)) (Vec x y) = (f x0 x1 x + f y0 y1 y) / squareSegmentLength s
  where
    f x0 x1 x = x0 * (x0 - x - x1) + x * x1

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

leftOf :: Point -> Segment -> Bool
leftOf p (Seg p0 p1) = dot (rot90 $ p - p0) (p1 - p0) < 0

dot :: Vec -> Vec -> Scalar
dot (Vec x1 y1) (Vec x2 y2) = x1 * x2 + y1 * y2

rot90 :: Vec -> Vec
rot90 (Vec x y) = Vec (-y) x

norm :: Vec -> Vec
norm v = v / abs v

angle :: Vec -> Vec -> Scalar
angle (Vec x1 y1) (Vec x2 y2)
  | α > β     = 2 * pi + β - α
  | otherwise = β - α
  where
    α = atan2 y1 x1
    β = atan2 y2 x2

-- Distances --------------------------------------------------------------

class DistanceToPoint a where
  distance       :: a -> Point -> Scalar
  squareDistance :: a -> Point -> Scalar
  distanceAtMost :: Scalar -> a -> Point -> Maybe Scalar

  distance x p       = sqrt (squareDistance x p)
  squareDistance x p = distance x p ^ 2

  distanceAtMost d x p = case distance x p of
    d' | d' <= d   -> Just d'
       | otherwise -> Nothing

instance DistanceToPoint Vec where
  squareDistance (Vec x0 y0) (Vec x1 y1) = (x1 - x0)^2 + (y1 - y0)^2

instance DistanceToPoint Segment where
  distance = distanceToSegment

-- Transformations --------------------------------------------------------

class Transformable a where
  transform :: (Point -> Point) -> a -> a

instance Transformable Vec where
  transform = id

instance Transformable Segment where
  transform f (Seg p q) = Seg (f p) (f q)

instance Transformable a => Transformable [a] where
  transform f = map (transform f)

instance Transformable () where
  transform _ x = x

instance (Transformable a, Transformable b) => Transformable (a, b) where
  transform f (x, y) = (transform f x, transform f y)

instance (Transformable a, Transformable b, Transformable c) => Transformable (a, b, c) where
  transform f (x, y, z) = (transform f x, transform f y, transform f z)

translate :: Transformable a => Vec -> a -> a
translate v = transform (v +)

scale :: Transformable a => Vec -> a -> a
scale v = transform (* v)

scaleFrom :: Transformable a => Point -> Vec -> a -> a
scaleFrom p v = translate p . scale v . translate (-p)

rotate :: Transformable a => Scalar -> a -> a
rotate α = transform (rot α)
  where
    rot α (Vec x y) = Vec (x * cs - y * sn) (x * sn + y * cs)
      where
        cs = cos α
        sn = sin α


rotateAround :: Transformable a => Point -> Scalar -> a -> a
rotateAround p α = translate p . rotate α . translate (-p)

interpolate :: Point -> Point -> Scalar -> Point
interpolate p q t = p + diag t * (q - p)

-- Basis ------------------------------------------------------------------

data Basis = Basis { origin, xUnit, yUnit :: Point }
  deriving (Show, Eq, Ord)

defaultBasis :: Basis
defaultBasis = Basis 0 (Vec 1 0) (Vec 0 1)

instance Transformable Basis where
  transform f (Basis o x y) = Basis (transform f o) (transform f x) (transform f y)

