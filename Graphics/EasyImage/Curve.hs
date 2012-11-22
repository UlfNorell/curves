{-# LANGUAGE MultiWayIf, ExistentialQuantification, BangPatterns, DeriveFunctor, GADTs,
             KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}
module Graphics.EasyImage.Curve where

import Control.Arrow ((***), (&&&))

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Attribute

import Debug.Trace

data Curves = Curves { curvePaths :: [Curve]
                     , curveStyle :: CurveStyle }

data Curve = forall a. Transformable a =>
             Curve { curveFunction :: Scalar -> a
                   , curveRender   :: Scalar -> a -> Point
                   , curveDensity  :: Int   -- ^ Number of subcurves that have been joined into this one
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
-- A set of closed curves combined with <Graphics-EasyImage.html#plusdotplus +.+>
-- can be filled using a fill colour ('transparent' for no fill). A point is
-- deemed inside the curves if a ray starting at the point intersects with the
-- curves an odd number of times. The fill blur is the width of the band around
-- the curve edge in which the fill colour fades to full transparency.  Setting
-- the fill colour of a non-closed curve results in unspecified behaviour.
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

instance HasAttribute a CurveStyle => HasAttribute a Curves where
  modifyAttribute attr f (Curves cs s) = Curves cs (modifyAttribute attr f s)

defaultCurveStyle =
  CurveStyle { lineWidth  = \_ _ -> 0.0
             , lineBlur   = \_ _ -> 1.2
             , lineColour = \_ _ -> black
             , fillColour = transparent
             , fillBlur   = sqrt 2 }

instance Transformable Curve where
  transform h (Curve f g n) = Curve (transform h f) g n

instance Transformable Curves where
  transform f (Curves cs s) = Curves (transform f cs) s

reverseCurve :: Curve -> Curve
reverseCurve (Curve f g n) = Curve f' g' n
  where
    f' t = f (1 - t)
    g' t p = g (1 - t) p

data Freezing = Freeze { freezeSize, freezeOrientation :: Bool }

-- | Freeze dimension to pixels.
freezeCurve :: Freezing -> Point -> Curve -> Curve
freezeCurve fr p0 (Curve f g n) = Curve (const basis) g' n
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

bindCurve :: Transformable a => (Scalar -> a) -> (Scalar -> a -> Curve) -> Curve
bindCurve f g = Curve f g' 1  -- not quite the right density
  where
    g' t x = case g t x of Curve i j _ -> j t (i t)

joinCurve :: Curves -> Curves -> Curves
joinCurve (Curves cs s) (Curves (c:cs2) _) =
  Curves (init cs ++ [joinCurve' (last cs) c] ++ cs2) s

joinCurve' :: Curve -> Curve -> Curve
joinCurve' (Curve f f' n) (Curve g g' m) =
    Curve h h' (n + m)
  where
    mid = fromIntegral n / fromIntegral (n + m)
    lo t = t / mid
    hi t = (t - mid) / (1 - mid)
    p = f 1
    q = g 0
    h t | t <= mid   = Left  $ f (lo t)
         | otherwise = Right $ g (hi t)
    h' t (Left p)  = f' (lo t) p
    h' t (Right p) = g' (hi t) p

appendPoint :: Curves -> Point -> Curves
appendPoint (Curves cs s) p = Curves (init cs ++ [appendPoint' (last cs) p]) s

appendPoint' :: Curve -> Point -> Curve
appendPoint' (Curve f g n) p = Curve f' g' (n + 1)
  where
    mid   = fromIntegral n / fromIntegral (n + 1)
    lo t  = t / mid
    hi t  = (t - mid) / (1 - mid)
    endPt = f 1
    f' t | t <= mid  = Left $ f (lo t)
         | otherwise = Right (endPt, p)
    g' t (Left p)       = g (lo t) p
    g' t (Right (p, q)) = interpolate (g 1 p) q (hi t)

prependPoint :: Point -> Curves -> Curves
prependPoint p (Curves (c:cs) s) = Curves (prependPoint' p c : cs) s

prependPoint' :: Point -> Curve -> Curve
prependPoint' p (Curve f g n) = Curve f' g' (n + 1)
  where
    mid     = 1 / fromIntegral (n + 1)
    lo t    = t / mid
    hi t    = (t - mid) / (1 - mid)
    startPt = f 0
    f' t | t <= mid  = Left (p, startPt)
         | otherwise = Right $ f (hi t)
    g' t (Left (p, q)) = interpolate p (g 0 q) (lo t)
    g' t (Right p)     = g (hi t) p

differentiateCurve :: Curve -> Curve
differentiateCurve (Curve f g n) = Curve (const f) g' n
  where
    eps  = 0.01
    eps2 = eps ^ 2
    minus a b = max (a - b) 0
    plus  a b = min (a + b) 1
    g' t f = norm (p1 - p0)
      where
        h t = g t (f t)
        p   = h t
        farEnough q = squareDistance p q > eps2
        (t0, p0) = maybe (0, h 0) id $ findThreshold (\e -> h (t - e)) farEnough 1.0e-6 0 t
        (t1, p1) = maybe (1, h 1) id $ findThreshold (\e -> h (t + e)) farEnough 1.0e-6 0 (1 - t)

zipCurves :: (Scalar -> Point -> Point -> Point) -> Curves -> Curves -> Curves
zipCurves f (Curves cs1 s) (Curves cs2 _) = Curves (zipWith (zipCurve f) cs1 cs2) s

zipCurve :: (Scalar -> Point -> Point -> Point) -> Curve -> Curve -> Curve
zipCurve h (Curve f1 g1 n) (Curve f2 g2 m) = Curve (f1 &&& f2) g' (max n m)
  where
    g' t (x, y) = h t (g1 t x) (g2 t y)

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
curveToSegments :: Scalar -> Curves -> BBTree (AnnotatedSegment (Scalar, Scalar))
curveToSegments r (Curves cs _) =
    buildBBTree $ concatMap toSegments cs
  where
    toSegments (Curve f g _) = annotate $ map (uncurry Seg . (snd *** snd)) $ concatMap (uncurry subdivide) ss
      where
        h t = g t (f t)
        res = r^2
        pairs xs = zip xs (tail xs)

        annotate ss = annotate' total 0 ss
          where
            total = sum $ map segmentLength ss

        annotate' tot !d (s:ss) = AnnSeg (d, d/tot) s : annotate' tot (d + segmentLength s) ss
        annotate' _ _ [] = []

        ss = pairs $ do
          let n = 20  -- minimum number of segments
          i <- [0..n]
          let t = fromIntegral i / fromIntegral n
          return (t, h t)

        subdivide x@(t0, p0) y@(t1, p1)
          | stop      = [(x, y)]
          | d > res   = subdivide (t0, p0) (t, p) ++ subdivide (t, p) (t1, p1)
          | otherwise = [(x, y)]
          where
            d = squareDistance p0 p1
            -- Using this instead of (==) due to serious weird shit
            x === y = decodeFloat x == decodeFloat y
            stop = t === t0 || t === t1 -- we've run out of precision
            sh (t, p) = "  f " ++ show t ++ " = " ++ show p
            t = (t0 + t1) / 2
            p = h t

