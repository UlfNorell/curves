{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Graphics.EasyImage.Math where

import Control.Applicative
import Test.QuickCheck

type Scalar = Double

data Vec = Vec { getX, getY :: !Scalar }
  deriving (Eq, Ord)

type Point = Vec

instance Show Vec where
  show (Vec x y) = "(" ++ show x ++ " " ++ show y ++ ")"

instance Num Vec where
  Vec a b + Vec c d = Vec (a + c) (b + d)
  Vec a b - Vec c d = Vec (a - c) (b - d)
  Vec a b * Vec c d = Vec (a * c) (b * d)
  negate (Vec a b)  = Vec (negate a) (negate b)
  abs v             = Vec (distance v 0) 0
  signum (Vec a b)  = Vec (atan2 a b) 0
  fromInteger n     = Vec (fromInteger n) 0

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
projectOnSegment s@(Seg (Vec x0 y0) (Vec x1 y1)) (Vec x y) = (f x0 x1 x + f y0 y1 y) / squareSegmentLength s
  where
    f x0 x1 x = x0 * (x0 - x - x1) + x * x1

rot :: Scalar -> Vec -> Vec
rot α (Vec x y) = Vec (x * cs - y * sn) (x * sn + y * cs)
  where
    cs = cos α
    sn = sin α

translatePt :: Point -> Point -> Point
translatePt (Vec x0 y0) (Vec x1 y1) = Vec (x0 + x1) (y0 + y1)

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

-- Testing ----------------------------------------------------------------

instance Arbitrary Vec where
  arbitrary = Vec <$> arbitrary <*> arbitrary
  shrink (Vec x y) =
    [ Vec x y | x <- shrink x ] ++
    [ Vec x y | y <- shrink y ]

chooseDouble :: (Scalar, Scalar) -> Gen Scalar
chooseDouble (a, b) = do
  let m = 1000000 :: Integer
  n <- choose (0, m)
  return (a + (b - a) * fromIntegral n / fromIntegral m)

x ≈ y = abs (x - y) < 1.0e-6

prop_distanceToSegment (Positive s) (Positive d) =
  forAllShrink (chooseDouble (-0.2, 1.2)) shrink $ \t ->
  forAllShrink (chooseDouble (0, 2 * pi)) shrink $ \α ->
  forAllShrink arbitrary shrink $ \dp ->
    let tr x y = translatePt dp $ rot α $ Vec x y
        p0 = tr 0 0
        p1 = tr s 0
        p  = tr (t * s) d
        dist = distance (Seg p0 p1) p
        ans | t <= 0    = distance p0 p
            | t >= 1    = distance p1 p
            | otherwise = d
    in whenFail (putStr $ unlines
                  [ "dist = " ++ show dist
                  , "ans  = " ++ show ans ]
                )
        (dist ≈ ans)

