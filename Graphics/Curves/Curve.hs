{-# LANGUAGE MultiWayIf, ExistentialQuantification, BangPatterns, DeriveFunctor, GADTs,
             KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}
module Graphics.Curves.Curve where

import Control.Arrow ((***), (&&&), first, second)

import Graphics.Curves.Math
import Graphics.Curves.Colour
import Graphics.Curves.BoundingBox
import Graphics.Curves.Attribute

import Debug.Trace

data Curves = Curves { curvePaths     :: [Curve]
                     , curveFillStyle :: CurveFillStyle }

data Curve = forall a. Transformable a =>
             Curve { curveFunction  :: Scalar -> a
                   , curveRender    :: Scalar -> a -> Point
                   , curveLineStyle :: Scalar -> Scalar -> Point -> CurveLineStyle
                   , curveDensity   :: Int   -- ^ Number of subcurves that have been joined into this one
                   }

data CurveLineStyle = CurveLineStyle
  { lineWidth    :: Scalar
  , lineBlur     :: Scalar
  , lineColour   :: Colour
  }

data CurveFillStyle = CurveFillStyle
  { fillColour   :: FillColour
  , textureBasis :: Basis
  , fillBlur     :: Scalar
  }

data FillColour = SolidFill Colour | TextureFill (Point -> Point -> Colour)

getFillColour :: FillColour -> Point -> Point -> Colour
getFillColour (SolidFill c)   _ _ = c
getFillColour (TextureFill t) p r = t p r

-- | Style attributes of a curve. The line width is with width in pixels of the
-- solid part of the curve. Outside the line width the curve fades to
-- full transparency in a band whose width is determined by the line blur
-- attribute.  All line attributes can be parameterized by the absolute (in
-- pixels) and relative distance from the start of the curve.
--
-- A set of closed curves combined with 'Graphics.Curves.+++'
-- can be filled using a fill colour ('transparent' for no fill) or a texture.
-- A texture is a function that computes a colour value given the position of the
-- point being filled, both in absolute pixels and relative to the texture
-- basis. The texture basis is 'defaultBasis' by default and is transformed
-- with the image. Typically you would use the absolute position for
-- rasterisation and the relative position for textures.
--
-- A point is deemed inside the curves if a ray starting at the point
-- intersects with the curves an odd number of times. The fill blur is the
-- width of the band around the curve edge in which the fill colour fades to
-- full transparency.  Setting the fill colour of a non-closed curve results in
-- unspecified behaviour.
data CurveAttribute :: * -> * where
  LineWidth     :: CurveAttribute Scalar
  LineBlur      :: CurveAttribute Scalar
  LineColour    :: CurveAttribute Colour
  VarLineWidth  :: CurveAttribute (Scalar -> Scalar -> Point -> Scalar)
  VarLineBlur   :: CurveAttribute (Scalar -> Scalar -> Point -> Scalar)
  VarLineColour :: CurveAttribute (Scalar -> Scalar -> Point -> Colour)
  FillBlur      :: CurveAttribute Scalar
  FillColour    :: CurveAttribute Colour
  Texture       :: CurveAttribute (Point -> Point -> Colour)
  TextureBasis  :: CurveAttribute Basis

instance HasAttribute CurveAttribute Curves where
  modifyAttribute a f =
    case a of
      LineWidth     -> onLine_ $ \s -> s { lineWidth  = f (lineWidth s) }
      LineBlur      -> onLine_ $ \s -> s { lineBlur   = f (lineBlur s) }
      LineColour    -> onLine_ $ \s -> s { lineColour = f (lineColour s) }
      VarLineWidth  -> onLine  $ \h d r p -> (h d r p) { lineWidth  = f (\d r p -> lineWidth  (h d r p)) d r p }
      VarLineBlur   -> onLine  $ \h d r p -> (h d r p) { lineBlur   = f (\d r p -> lineBlur   (h d r p)) d r p }
      VarLineColour -> onLine  $ \h d r p -> (h d r p) { lineColour = f (\d r p -> lineColour (h d r p)) d r p }
      FillBlur      -> onFill  $ \s -> s { fillBlur     = f $ fillBlur s }
      TextureBasis  -> onFill  $ \s -> s { textureBasis = f $ textureBasis s }
      FillColour    -> onFill  $ \s ->
        s { fillColour = case fillColour s of
                           SolidFill c   -> SolidFill (f c)
                           TextureFill t -> TextureFill $ \p r -> f (t p r)
          }
      Texture       -> onFill  $ \s -> s { fillColour = TextureFill $ f (tex s) }
        where tex s = case fillColour s of
                        SolidFill c   -> \_ _ -> c
                        TextureFill t -> t
    where
      onFill :: (CurveFillStyle -> CurveFillStyle) -> Curves -> Curves
      onFill f (Curves cs fill) = Curves cs (f fill)

      onLine_ :: (CurveLineStyle -> CurveLineStyle) -> Curves -> Curves
      onLine_ f = onLine (\old d r p -> f (old d r p))

      onLine :: ((Scalar -> Scalar -> Point -> CurveLineStyle) -> (Scalar -> Scalar -> Point -> CurveLineStyle)) -> Curves -> Curves
      onLine f (Curves cs fill) = Curves (map onCurve cs) fill
        where
          onCurve c@(Curve g h old n) = Curve g h (\d r p -> f old d r p) n

  setAttribute FillColour c (Curves cs fill) = Curves cs fill{ fillColour = SolidFill c }  -- allows turning a Texture into a Solid
  setAttribute a x s = modifyAttribute a (const x) s

defaultCurveLineStyle =
  CurveLineStyle
    { lineWidth    = 0.0
    , lineBlur     = 1.2
    , lineColour   = black
    }

defaultCurveFillStyle =
  CurveFillStyle
    { fillColour   = SolidFill transparent
    , fillBlur     = sqrt 2
    , textureBasis = defaultBasis
    }

instance Transformable Curve where
  transform h (Curve f g st n) = Curve (transform h f) g st n

instance Transformable Curves where
  transform f (Curves cs s) = Curves (transform f cs) (transform f s)

instance Transformable CurveFillStyle where
  transform f s = s { textureBasis = transform f $ textureBasis s }

reverseCurve :: Curve -> Curve
reverseCurve (Curve f g st n) = Curve f' g' st n  -- doesn't reverse style!
  where
    f' t = f (1 - t)
    g' t p = g (1 - t) p

data Freezing = Freeze { freezeSize, freezeOrientation :: Bool }

-- | Freeze dimension to pixels.
freezeCurve :: Freezing -> Point -> Curve -> Curve
freezeCurve fr p0 (Curve f g st n) = Curve (const basis) g' st n
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
bindCurve f g = Curve f g' style 1  -- not quite the right density
  where
    g' t x    = case g t x of Curve i j _ _ -> j t (i t)
    style d r = case g r (f r) of Curve _ _ st _ -> st d r  -- is this right?

joinCurve :: Curves -> Curves -> Curves
joinCurve (Curves cs s) (Curves (c:cs2) _) =
  Curves (init cs ++ [joinCurve' (last cs) c] ++ cs2) s

joinCurve' :: Curve -> Curve -> Curve
joinCurve' (Curve f f' st n) (Curve g g' _ m) =
    Curve h h' st (n + m)
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
appendPoint' (Curve f g st n) p = Curve f' g' st (n + 1)
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
prependPoint' p (Curve f g st n) = Curve f' g' st (n + 1)
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
differentiateCurve (Curve f g st n) = Curve (const f) g' st n
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
zipCurve h (Curve f1 g1 st1 n) (Curve f2 g2 _ m) = Curve (f1 &&& f2) g' st1 (max n m)
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

-- Each segment is annotated with the distance from the start of the curve and the line style
curveToSegments :: Scalar -> Curves -> BBTree (AnnotatedSegment (Scalar, Scalar, CurveLineStyle))
curveToSegments r (Curves cs _) = buildBBTree $ concatMap (toSegments r) cs

curveLength' :: Scalar -> Curve -> Scalar
curveLength' r c = d + segmentLength s
  where AnnSeg (d, _, _) s = last $ toSegments r c

toSegments :: Scalar -> Curve -> [AnnotatedSegment (Scalar, Scalar, CurveLineStyle)]
toSegments r (Curve f g style _) =
    annotate $ map mkSeg $ concatMap (uncurry subdivide) ss
  where
    h t = g t (f t)
    res = r^2
    pairs xs = zip xs (tail xs)

    mkSeg ((_, p0), (_, p1)) = Seg p0 p1

    annotate ss = annotate' total 0 ss
      where
        total = sum $ map segmentLength ss

    addDist d r style = (d, r, style)

    annotate' tot !d (s@(Seg p0 p1):ss) = AnnSeg (d, d/tot, style d r p) s : annotate' tot (d + segmentLength s) ss
      where p = (p0 + p1) / 2

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

