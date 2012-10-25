
module Graphics.EasyImage.Image where

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve
import Graphics.EasyImage.BoundingBox

-- Two representations:
--  - curve representation
--  - segments representation
--
-- Work with curve representation until just before rasterization. Then compile
-- to segment representation.

-- TODO: - explicit transformation matrices (for effiecency?)
--       - intersections
data Image = ICurve Curve
           | Union BlendFunc [Image]

-- Image operations -------------------------------------------------------

(<>) :: Image -> Image -> Image
i1 <> i2 = Union defaultBlendFunc [i1, i2]

curve :: (Scalar -> Point) -> Scalar -> Scalar -> Image
curve f t0 t1 = ICurve $ Curve f t0 t1 defaultCurveStyle

withStyle :: Image -> CurveStyle -> Image
withStyle i s = onStyle i $ const s

onStyle :: Image -> (CurveStyle -> CurveStyle) -> Image
onStyle (ICurve curve) f   = ICurve $ curve { curveStyle = f $ curveStyle curve }
onStyle (Union blend is) f = Union blend $ map (`onStyle` f) is

withColour :: Image -> Colour -> Image
withColour i c = onStyle i $ \s -> s { lineColour = c }

transform :: (Point -> Point) -> Image -> Image
transform f (ICurve c) = ICurve c { curveFunction = f . curveFunction c }
transform f (Union b is) = Union b $ map (transform f) is

scale' :: Vec -> Image -> Image
scale' v = transform (v *)

scale k = scale' (Vec k k)

translate :: Vec -> Image -> Image
translate v = transform (v +)

rotate :: Scalar -> Image -> Image
rotate α = transform (rot α)

rotateAround :: Scalar -> Point -> Image -> Image
rotateAround α v = translate v . rotate α . translate (-v)

