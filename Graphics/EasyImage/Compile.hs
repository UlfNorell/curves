
module Graphics.EasyImage.Compile where

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve

-- Compilation ------------------------------------------------------------

type Segments = BBTree Segment

data CompiledImage
      = Segments CurveStyle Segments
      | CIUnion BlendFunc (BBTree CompiledImage)

instance HasBoundingBox CompiledImage where
  bounds (Segments s b) = relaxBoundingBox (max (fillBlur s) $ lineWidth s + lineBlur s) $ bounds b
  bounds (CIUnion _ b)  = bounds b

compileImage :: Image -> CompiledImage
compileImage = compileImage' 1

compileImage' :: Scalar -> Image -> CompiledImage
compileImage' res (ICurve c) = Segments (curveStyle c) (curveToSegments res c)
compileImage' res (Union blend is) =
  CIUnion blend $ buildBBTree $ map (compileImage' res) is

autoFit :: Point -> Point -> Image -> Image
autoFit p0 p1 i =
  translate (p0 - q0 + offs) $ scaleFrom q0 k i
  where
    zap = [ LineWidth 1.0e-14, LineBlur 0, FillBlur 0 ]
    Seg q0 q1 = bboxToSegment $ bounds $ compileImage' 0.001 $ i `with` zap
    screen = p1 - p0
    world  = q1 - q0
    k      = diag $ vuncurry min (screen / world)
    world' = k * world
    offs   = 0.5 * (screen - world')

