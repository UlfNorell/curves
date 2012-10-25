
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
  bounds (Segments s b) = relaxBoundingBox (lineWidth s + lineBlur s) $ bounds b
  bounds (CIUnion _ b)  = bounds b

compileImage :: Image -> CompiledImage
compileImage (ICurve c) = Segments (curveStyle c) (curveToSegments 1 c)
compileImage (Union blend is) =
  CIUnion blend $ buildBBTree $ map compileImage is

