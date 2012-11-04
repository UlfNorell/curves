{-# LANGUAGE MultiWayIf, ExistentialQuantification, BangPatterns, DeriveFunctor #-}
module Graphics.EasyImage.Curve where

import Control.Arrow ((***))

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.BoundingBox

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

data Attr = LineWidth  Scalar
          | LineBlur   Scalar
          | LineColour Colour
          | VarLineWidth  (Scalar -> Scalar -> Scalar)
          | VarLineBlur   (Scalar -> Scalar -> Scalar)
          | VarLineColour (Scalar -> Scalar -> Colour)
          | FillBlur   Scalar
          | FillColour Colour

setAttr :: Attr -> CurveStyle -> CurveStyle
setAttr (LineWidth x)     s = s { lineWidth = \_ _ -> x }
setAttr (LineBlur x)      s = s { lineBlur = \_ _ -> x }
setAttr (LineColour x)    s = s { lineColour = \_ _ -> x }
setAttr (VarLineWidth x)     s = s { lineWidth = x }
setAttr (VarLineBlur x)      s = s { lineBlur = x }
setAttr (VarLineColour x) s = s { lineColour = x }
setAttr (FillColour x)    s = s { fillColour = x }
setAttr (FillBlur x)      s = s { fillBlur = x }

defaultCurveStyle =
  CurveStyle { lineWidth  = \_ _ -> 0.0
             , lineBlur   = \_ _ -> 1.2
             , lineColour = \_ _ -> black
             , fillColour = transparent
             , fillBlur   = 1.2 }

lineStyle w b c = [LineWidth w, LineBlur b, LineColour c]
fillStyle b c   = [FillColour c, FillBlur b]

instance Transformable Curve where
  transform h (Curve f g t0 t1 s) = Curve (transform h . f) g t0 t1 s

straightLine :: Point -> Point -> Curve
straightLine p q = Curve (interpolate p q) (const id) 0 1 defaultCurveStyle

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
            SecondPart p -> g' t p
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

prependPoint :: Point -> Curve -> Curve
prependPoint p (Curve f g a b s) = Curve f' g' (a - 1) b s
  where
    startPt = f a
    f' t | t >= a    = SecondPart (f t)
         | otherwise = Gap p startPt
    g' t (Gap p q)      = interpolate p (g a q) (t - a + 1)
    g' t (SecondPart p) = g t p

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
    buildBBTree $ annotate $ map (uncurry Seg . (snd *** snd)) $ concatMap subdivide ss
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

    subdivide s@((t0, p0), (t1, p1))
      | squareDistance p0 p1 > res = concatMap subdivide [((t0, p0), (t, p)), ((t, p), (t1, p1))]
      | otherwise                  = [s]
      where
        t = (t0 + t1) / 2
        p = h t

