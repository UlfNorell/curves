{-# LANGUAGE MultiWayIf, ExistentialQuantification, BangPatterns, DeriveFunctor, GADTs,
             KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}
module Graphics.EasyImage.Curve where

import Control.Arrow ((***))

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Attribute

data Curve = forall a. Transformable a =>
             Curve { curveFunction :: Scalar -> a
                   , curveRender   :: Scalar -> a -> Point
                   , curveStart    :: Scalar
                   , curveEnd      :: Scalar
                   , curveStyle    :: CurveStyle
                   }

data CurveStyle = CurveStyle
      { lineWidth  :: Scalar -> Scalar -> Scalar
      , lineBlur   :: Scalar -> Scalar -> Scalar
      , lineColour :: Scalar -> Scalar -> Colour
      , fillColour :: Colour
      , fillBlur   :: Scalar
      }

-- | Style attributes of a curve. The line width is with width in pixels of the
-- solid part of the curve. Outside the line width the curve fades to
-- full transparency in a band whose width is determined by the line blur
-- attribute.  All line attributes can be parameterized by the absolute (in
-- pixels) and relative distance from the start of the curve.
--
-- Closed curves can be filled using a fill colour ('transparent' for no fill).
-- The fill blur is the width of the band outside the curve in which the fill
-- colour fades to full transparency. Setting the fill colour of a non-closed
-- curve results in unspecified behaviour.
data CurveAttribute :: * -> * where
  LineWidth     :: CurveAttribute Scalar
  LineBlur      :: CurveAttribute Scalar
  LineColour    :: CurveAttribute Colour
  VarLineWidth  :: CurveAttribute (Scalar -> Scalar -> Scalar)
  VarLineBlur   :: CurveAttribute (Scalar -> Scalar -> Scalar)
  VarLineColour :: CurveAttribute (Scalar -> Scalar -> Colour)
  FillBlur      :: CurveAttribute Scalar
  FillColour    :: CurveAttribute Colour

instance HasAttribute CurveAttribute CurveStyle where
  modifyAttribute LineWidth     f s = s { lineWidth  = \d r -> f (lineWidth s d r) }
  modifyAttribute LineBlur      f s = s { lineBlur   = \d r -> f (lineBlur s d r) }
  modifyAttribute LineColour    f s = s { lineColour = \d r -> f (lineColour s d r) }
  modifyAttribute VarLineWidth  f s = s { lineWidth  = f $ lineWidth s }
  modifyAttribute VarLineBlur   f s = s { lineBlur   = f $ lineBlur s }
  modifyAttribute VarLineColour f s = s { lineColour = f $ lineColour s }
  modifyAttribute FillColour    f s = s { fillColour = f $ fillColour s }
  modifyAttribute FillBlur      f s = s { fillBlur   = f $ fillBlur s }

instance HasAttribute a CurveStyle => HasAttribute a Curve where
  modifyAttribute attr f c = c { curveStyle = modifyAttribute attr f $ curveStyle c }

defaultCurveStyle =
  CurveStyle { lineWidth  = \_ _ -> 0.0
             , lineBlur   = \_ _ -> 1.2
             , lineColour = \_ _ -> black
             , fillColour = transparent
             , fillBlur   = 1.2 }

instance Transformable Curve where
  transform h (Curve f g t0 t1 s) = Curve (transform h . f) g t0 t1 s

reverseCurve :: Curve -> Curve
reverseCurve (Curve f g a b s) = Curve f' g' a b s
  where
    f' t = f (b + a - t)
    g' t p = g (b + a - t) p

data Freezing = Freeze { freezeSize, freezeOrientation :: Bool }

-- | Freeze dimension to pixels.
freezeCurve :: Freezing -> Point -> Curve -> Curve
freezeCurve fr p0 (Curve f g a b s) = Curve (const basis) g' a b s
  where
    fsize = freezeSize fr
    fdir  = freezeOrientation fr
    basis = translate p0 defaultBasis

    g' t (Basis o px py) = g t (transform h (f t))
      where
        h q = o + diag (getX v) * vx + diag (getY v) * vy
          where v = q - p0
        (vx, vy)
          | fdir && fsize = (Vec 1 0, Vec 0 1)
          | fdir          = (Vec (distance px o) 0, Vec 0 (distance py o))
          | fsize         = (norm (px - o), norm (py - o))

data Join a b = FirstPart a | Gap a b | SecondPart b

instance (Transformable a, Transformable b) => Transformable (Join a b) where
  transform f (FirstPart a)  = FirstPart $ transform f a
  transform f (Gap p q)      = Gap (transform f p) (transform f q)
  transform f (SecondPart b) = SecondPart $ transform f b

joinCurve :: Curve -> Curve -> Curve
joinCurve (Curve f f' t0 t1 s) (Curve g g' s0 s1 _) =
    Curve (\ t ->
              if | t <= t1     -> FirstPart $ f t
                 | t <= t1 + 1 -> Gap p q
                 | otherwise   -> SecondPart $ g (t - t1 + s0 - 1))
          (\ t r -> case r of
            FirstPart p  -> f' t p
            Gap p q      -> interpolate (f' t1 p) (g' s0 q) (t - t1)
            SecondPart p -> g' (t - t1 + s0 - 1) p
          )
          t0 (t1 + s1 - s0 + 1) s
  where
    p = f t1
    q = g s0

appendPoint :: Curve -> Point -> Curve
appendPoint (Curve f g a b s) p = Curve f' g' a (b + 1) s
  where
    endPt = f b
    f' t | t <= b    = FirstPart (f t)
         | otherwise = Gap endPt p
    g' t (FirstPart p) = g t p
    g' t (Gap p q)     = interpolate (g b p) q (t - b)
    g' t SecondPart{}  = error "appendPoint: impossible"

prependPoint :: Point -> Curve -> Curve
prependPoint p (Curve f g a b s) = Curve f' g' (a - 1) b s
  where
    startPt = f a
    f' t | t >= a    = SecondPart (f t)
         | otherwise = Gap p startPt
    g' t (Gap p q)      = interpolate p (g a q) (t - a + 1)
    g' t (SecondPart p) = g t p
    g' t FirstPart{}    = error "prependPoint: impossible"

data AnnotatedSegment a = AnnSeg { annotation :: a
                                 , theSegment :: Segment }
  deriving (Functor)

instance HasBoundingBox (AnnotatedSegment a) where
  bounds = bounds . theSegment

instance DistanceToPoint (AnnotatedSegment a) where
  distanceAtMost d = distanceAtMost d . theSegment
  distance         = distance . theSegment
  squareDistance   = squareDistance . theSegment

-- Each segment is annotated with the distance from the start of the curve.
curveToSegments :: Scalar -> Curve -> BBTree (AnnotatedSegment (Scalar, Scalar))
curveToSegments r (Curve f g t0 t1 _) =
    buildBBTree $ annotate $ map (uncurry Seg . (snd *** snd)) $ concatMap (uncurry subdivide) ss
  where
    h t = g t (f t)
    res = r^2
    n = 20  -- minimum number of segments
    pairs xs = zip xs (tail xs)

    annotate ss = annotate' total 0 ss
      where
        total = sum $ map segmentLength ss

    annotate' tot !d (s:ss) = AnnSeg (d, d/tot) s : annotate' tot (d + segmentLength s) ss
    annotate' _ _ [] = []

    ss = pairs $ do
      i <- [0..n]
      let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral n
      return (t, h t)

    subdivide x@(t0, p0) y@(t1, p1)
      | squareDistance p0 p1 > res = subdivide (t0, p0) (t, p) ++ subdivide (t, p) (t1, p1)
      | otherwise                  = [(x, y)]
      where
        t = (t0 + t1) / 2
        p = h t

