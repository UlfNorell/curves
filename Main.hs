
import System.Environment

import Graphics.EasyImage

circle :: Point -> Scalar -> Image
circle (Vec x y) r =
  curve (\α -> Vec (x + r * cos α) (y + r * sin α)) 0 (2 * pi) `withStyle` CurveStyle 1 1.2 red

ellipse :: Point -> Scalar -> Scalar -> Image
ellipse (Vec x y) r d = curve f 0 (2 * pi) `withStyle` CurveStyle 3 1.2 (transparency 0.7 blue)
  where
    f α = Vec (x + (r - s) * cos α) (y + (r - s) * sin α)
      where
        s = (d^2 + r^2 - 2 * d * r * cos α) / (2 * r - 2 * d * cos α)

save i = renderImage "test.png" 800 600 white i

main =
  save $ rotateAround (pi/6) (Vec 400 325) (ellipse (Vec 400 300) 400 350) <>
         translate (Vec 250 300) (scale 200 $ circle (Vec 0 0) 1)

-- TODO
--
--  * filling curves
--    - How: take a point outside the bounding box
--           compute number of intersections with a curve segment
--           odd: inside, even: outside

