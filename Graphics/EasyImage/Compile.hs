
module Graphics.EasyImage.Compile where

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve

import Debug.Trace

-- Compilation ------------------------------------------------------------

type Segments = BBTree SegmentAndDistance

data CompiledImage
      = Segments CurveStyle Segments
      | CIUnion BlendFunc (BBTree CompiledImage)
      | CIEmpty

instance HasBoundingBox CompiledImage where
  bounds (Segments s b) = relaxBoundingBox (max (fillBlur s) $ lineWidth s + lineBlur s) $ bounds b
  bounds (CIUnion _ b)  = bounds b
  bounds CIEmpty        = Empty

compileImage :: Image -> CompiledImage
compileImage = compileImage' 1

compileImage' :: Scalar -> Image -> CompiledImage
compileImage' res (ICurve c) = Segments (curveStyle c) (curveToSegments res c)
compileImage' res (Union _ []) = CIEmpty
compileImage' res (Union blend is) =
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

