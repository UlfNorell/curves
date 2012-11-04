{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Graphics.EasyImage.BoundingBox where

import Prelude hiding (minimum, maximum, any, or, and)
import Control.Applicative
import Data.Monoid
import Data.Function
-- import Data.List
import Data.Foldable hiding (concatMap)
import Test.QuickCheck

import Graphics.EasyImage.Math

-- Bounding boxes ---------------------------------------------------------

data BoundingBox = BBox !Scalar !Scalar !Scalar !Scalar  -- x0 y0 x1 y1
                 | Empty
  deriving (Show, Eq, Ord)

instance Monoid BoundingBox where
  mempty = Empty
  mappend Empty b = b
  mappend b Empty = b
  mappend (BBox x0 y0 x1 y1) (BBox x2 y2 x3 y3) =
    BBox (min x0 x2) (min y0 y2) (max x1 x3) (max y1 y3)

class HasBoundingBox a where
  bounds :: a -> BoundingBox

instance HasBoundingBox BoundingBox where
  bounds = id

{-# INLINE insideBBox #-}
insideBBox :: Point -> BoundingBox -> Bool
insideBBox (Vec x y) (BBox x0 y0 x1 y1) =
  x0 <= x && x <= x1 &&
  y0 <= y && y <= y1

segmentToBBox :: Segment -> BoundingBox
segmentToBBox (Seg p1 p2) =
  BBox ((min `on` getX) p1 p2)
       ((min `on` getY) p1 p2)
       ((max `on` getX) p1 p2)
       ((max `on` getY) p1 p2)

bboxToSegment :: BoundingBox -> Segment
bboxToSegment (BBox x0 y0 x1 y1) = Seg (Vec x0 y0) (Vec x1 y1)
bboxToSegment Empty              = Seg 0 0

instance HasBoundingBox Segment where
  bounds = segmentToBBox

instance DistanceToPoint BoundingBox where
  -- Note: cheats in the corner cases
  distance (BBox x0 y0 x1 y1) (Vec x y)
    = 0 `max` (x0 - x) `max` (x - x1) `max` (y0 - y) `max` (y - y1)

relaxBoundingBox :: Scalar -> BoundingBox -> BoundingBox
relaxBoundingBox a (BBox x0 y0 x1 y1) = BBox (x0 - a) (y0 - a) (x1 + a) (y1 + a)

intersectBoundingBox :: Segment -> BoundingBox -> Bool
intersectBoundingBox (Seg p@(Vec px py) q@(Vec qx qy)) b@(BBox x0 y0 x1 y1)
  | py == qy = py >= y0 && py <= y1 &&
               (px >= x0 || qx >= x0) &&
               (px <= x1 || py <= x1)
intersectBoundingBox (Seg p0 p1) b@(BBox x0 y0 x1 y1)
  | getX p0 < x0 && getX p1 < x0       = False
  | getY p0 < y0 && getY p1 < y0       = False
  | getX p0 > x1 && getX p1 > x1       = False
  | getY p0 > y1 && getY p1 > y1       = False
  | insideBBox p0 b || insideBBox p1 b = True
  | otherwise =
    or [ dy /= 0 && any (inrange x0 x1) [ix1, ix2]
       , dx /= 0 && any (inrange y0 y1) [iy1, iy2]
       ]
  where
    Vec dx dy = p1 - p0
    isect x0 y0 dx dy y = x0 + dx * (y - y0) / dy
    inrange a b x = a <= x && x <= b
    ix1 = isect (getX p0) (getY p0) dx dy y0
    ix2 = isect (getX p0) (getY p0) dx dy y1
    iy1 = isect (getY p0) (getX p0) dy dx x0
    iy2 = isect (getY p0) (getX p0) dy dx x1

-- Bounding box trees -----------------------------------------------------

data BBTree a = Leaf a | Node BoundingBox (BBTree a) (BBTree a)
  deriving (Functor, Foldable, Eq, Show)

instance HasBoundingBox a => HasBoundingBox (BBTree a) where
  bounds (Leaf x)     = bounds x
  bounds (Node b _ _) = b

instance DistanceToPoint a => DistanceToPoint (BBTree a) where
  distance (Leaf x)     p = distance x p
  distance (Node _ l r) p = {-# SCC "distance@BBTree" #-} min (distance l p) (distance r p)
    -- could be optimized (looking at bounding boxes of l and r),
    -- but not a bottle-neck

  distanceAtMost d t p = fst <$> distanceAtMost' d t p

distanceAtMost' :: DistanceToPoint a => Scalar -> BBTree a -> Point -> Maybe (Scalar, a)
distanceAtMost' d (Leaf x)     p = (,) <$> distanceAtMost d x p <*> pure x
distanceAtMost' d (Node b l r) p =
  distanceAtMost d b p *>
  case (distanceAtMost' d l p, distanceAtMost' d r p) of
    (Nothing, d) -> d
    (d, Nothing) -> d
    (l@(Just (dl, x)), r@(Just (dr, y)))
      | dl < dr   -> l
      | otherwise -> r

buildBBTree :: HasBoundingBox a => [a] -> BBTree a
buildBBTree []  = error "buildBBTree []"
buildBBTree xs = loop (length xs) xs
  where
    loop _ [x] = Leaf x
    loop n xs  = Node ((mappend `on` bounds) l r) l r
      where
        n'       = div n 2
        (ys, zs) = splitAt n' xs
        l        = loop n' ys
        r        = loop (n - n') zs

intersectBBTree :: (Segment -> a -> [Point]) -> Segment -> BBTree a -> [Point]
intersectBBTree isect s (Leaf x) = isect s x
intersectBBTree isect s (Node b l r)
  | intersectBoundingBox s b = intersectBBTree isect s l ++ intersectBBTree isect s r
  | otherwise                = []

-- Testing ----------------------------------------------------------------

prop_intersectBBox v u (Positive k) (Positive t) =
  getY v /= 0 ==>
  (forAllShrink (chooseDouble (0, 2 * pi))  shrink $ \α ->
   forAllShrink (chooseDouble (0.01, 0.99)) shrink $ \x ->
   let pt p = u + Vec k k * rot α p
       p0 = pt $ Vec 0 0
       p1 = pt $ Vec 0 1
       i  = pt $ Vec 0 x
       q0 = pt $ Vec 0 x - v
       q1 = pt $ Vec 0 x + (Vec t t * v)
       l1 = Seg p0 p1
       l2 = Seg q0 q1
   in
    whenFail (putStr $ unlines [ "l1  = " ++ show l1
                               , "l2  = " ++ show l2
                               , "i   = " ++ show i ]) $
      intersectBoundingBox l1 (bounds l2)
  )

