{-# LANGUAGE DeriveFunctor #-}
module Graphics.EasyImage.BoundingBox where

import Control.Applicative
import Data.Monoid
import Data.Function

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

insideBBox :: Point -> BoundingBox -> Bool
insideBBox (Vec x y) (BBox x0 y0 x1 y1) =
  and [ x0 <= x, x <= x1, y0 <= y, y <= y1 ]

segmentToBBox :: Segment -> BoundingBox
segmentToBBox (Seg p1 p2) =
  BBox ((min `on` getX) p1 p2)
       ((min `on` getY) p1 p2)
       ((max `on` getX) p1 p2)
       ((max `on` getY) p1 p2)

instance HasBoundingBox Segment where
  bounds = segmentToBBox

instance DistanceToPoint BoundingBox where
  -- Note: cheats in the corner cases
  distance (BBox x0 y0 x1 y1) (Vec x y)
    = maximum [0, x0 - x, x - x1, y0 - y, y - y1]

relaxBoundingBox :: Scalar -> BoundingBox -> BoundingBox
relaxBoundingBox a (BBox x0 y0 x1 y1) = BBox (x0 - a) (y0 - a) (x1 + a) (y1 + a)

-- Bounding box trees -----------------------------------------------------

data BBTree a = Leaf a | Node BoundingBox [BBTree a]
  deriving (Functor, Eq, Show)

instance HasBoundingBox a => HasBoundingBox (BBTree a) where
  bounds (Leaf x)   = bounds x
  bounds (Node b _) = b

instance DistanceToPoint a => DistanceToPoint (BBTree a) where
  distance (Leaf x)    p = distance x p
  distance (Node _ bs) p = minimum $ map (`distance` p) bs  -- could be optimized

  distanceAtMost d (Leaf x)    p = distanceAtMost d x p
  distanceAtMost d (Node b ts) p =
    distanceAtMost d b p *> mins (map (\t -> distanceAtMost d t p) ts)
    where
      mins ds = case [ d | Just d <- ds ] of
                  [] -> Nothing
                  ds -> Just $ minimum ds

buildBBTree :: HasBoundingBox a => [a] -> BBTree a
buildBBTree []  = error "buildBBTree []"
buildBBTree [x] = Leaf x
buildBBTree xs  = Node ((mappend `on` bounds) l r) [l, r]
  where
    n        = div (length xs) 2
    (ys, zs) = splitAt n xs
    l        = buildBBTree ys
    r        = buildBBTree zs

