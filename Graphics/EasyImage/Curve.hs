{-# LANGUAGE MultiWayIf, ExistentialQuantification, BangPatterns #-}
module Graphics.EasyImage.Curve where

import Control.Arrow ((***))

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.BoundingBox

data Basis = Basis { origin, xUnit, yUnit :: Point }
  deriving (Show, Eq, Ord)

defaultBasis = Basis 0 (Vec 1 0) (Vec 0 1)

instance Transformable Basis where
  transform f (Basis o x y) = Basis (transform f o) (transform f x) (transform f y)

data Curve = forall a. Transformable a =>
             Curve { curveFunction :: Scalar -> a
                   , curveRender   :: Scalar -> a -> Point
                   , curveStart    :: Scalar
                   , curveEnd      :: Scalar
                   , curveStyle    :: CurveStyle
                   }

data CurveStyle = CurveStyle
      { lineWidth  :: Scalar
      , lineBlur   :: Scalar
      , lineColour :: Scalar -> Colour
      , fillColour :: Colour
      , fillBlur   :: Scalar
      }

data Attr = LineWidth  Scalar
          | LineBlur   Scalar
          | LineColour Colour
          | VarLineColour (Scalar -> Colour)
          | FillBlur   Scalar
          | FillColour Colour

setAttr :: Attr -> CurveStyle -> CurveStyle
setAttr (LineWidth x)     s = s { lineWidth = x }
setAttr (LineBlur x)      s = s { lineBlur = x }
setAttr (LineColour x)    s = s { lineColour = const x }
setAttr (VarLineColour x) s = s { lineColour = x }
setAttr (FillColour x)    s = s { fillColour = x }
setAttr (FillBlur x)      s = s { fillBlur = x }

defaultCurveStyle =
  CurveStyle { lineWidth  = 0.0
             , lineBlur   = 1.2
             , lineColour = const black
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

data SegmentAndDistance = SegAndDist { distanceFromStart :: Scalar
                                     , theSegment        :: Segment }

instance HasBoundingBox SegmentAndDistance where
  bounds = bounds . theSegment

instance DistanceToPoint SegmentAndDistance where
  distanceAtMost d = distanceAtMost d . theSegment
  distance         = distance . theSegment
  squareDistance   = squareDistance . theSegment

-- Each segment is annotated with the distance from the start of the curve.
curveToSegments :: Scalar -> Curve -> BBTree SegmentAndDistance
curveToSegments r (Curve f g t0 t1 _) =
    buildBBTree $ annotate 0 $ map (uncurry Seg . (snd *** snd)) $ concatMap subdivide ss
  where
    h t = g t (f t)
    res = r^2
    n = 20  -- minimum number of segments
    pairs xs = zip xs (tail xs)

    annotate !d (s:ss) = SegAndDist d s : annotate (d + segmentLength s) ss
    annotate _ [] = []

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

