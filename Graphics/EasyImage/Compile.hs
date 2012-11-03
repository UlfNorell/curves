
module Graphics.EasyImage.Compile where

import Prelude hiding (minimum, maximum, any, or, and)
import Control.Applicative
import Data.Foldable
import Data.Monoid

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve

import Debug.Trace

-- Compilation ------------------------------------------------------------

type Segments = BBTree (AnnotatedSegment LineStyle)

data FillStyle = FillStyle Colour Scalar LineStyle
data LineStyle = LineStyle Colour Scalar Scalar

instance Monoid LineStyle where
  mempty  = LineStyle transparent 0 0
  mappend (LineStyle c1 w1 b1) (LineStyle c2 w2 b2) =
    LineStyle (c1 `blend` c2) (max w1 w2) (max b1 b2)

data CompiledImage
      = Segments FillStyle Segments
      | CIUnion (Op (Maybe Colour)) (BBTree CompiledImage)
      | CIEmpty

instance HasBoundingBox CompiledImage where
  bounds (Segments fs b) = relaxBoundingBox (max fw lw) $ bounds b
    where
      fw = case fs of
             FillStyle c w _ | not $ isZero (getAlpha c) -> w
             _ -> 0
      lw = case fs of
             FillStyle _ _ (LineStyle c w b) | not $ isZero (getAlpha c) -> w + b
             _ -> 0
  bounds (CIUnion _ b)  = bounds b
  bounds CIEmpty        = Empty

compileImage :: Image -> CompiledImage
compileImage = compileImage' 1

setLineStyle :: CurveStyle -> SegmentAndDistance -> AnnotatedSegment LineStyle
setLineStyle s seg = fmap mkLineStyle seg
  where
    mkLineStyle d = LineStyle (lineColour s d) (lineWidth s d) (lineBlur s d)

compileImage' :: Scalar -> Image -> CompiledImage
compileImage' res (ICurve c) = Segments fs ss
  where
    s  = curveStyle c
    fs = FillStyle (fillColour s) (fillBlur s) (foldMap annotation ss)
    ss = setLineStyle (curveStyle c) <$> curveToSegments res c
compileImage' res (Combine _ []) = CIEmpty
compileImage' res (Combine blend is) =
  CIUnion blend $ buildBBTree $ map (compileImage' res) is

autoFit p q = loop . scale 1000
  where
    -- Repeat autoFit until reasonably stable. This makes it work for features
    -- that are scaling insensitive (line widths and frozen images).
    -- Scaling up by 1000 first makes it converge faster
    loop i
      | abs (k - 1) < 0.01 = i'
      | otherwise          = loop i'
      where
        (k, i') = autoFit' p q i

autoFit' :: Point -> Point -> Image -> (Scalar, Image)
autoFit' p0 p1 i =
  (getX k, translate (p0 - q0 + offs) $ scaleFrom q0 k i)
  where
    Seg q0 q1 = bboxToSegment $ bounds $ compileImage i
    screen = p1 - p0
    world  = q1 - q0
    k      = diag $ vuncurry min (screen / world)
    world' = k * world
    offs   = 0.5 * (screen - world')

