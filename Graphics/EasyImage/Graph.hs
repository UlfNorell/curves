
module Graphics.EasyImage.Graph where

import Graphics.EasyImage
import Graphics.EasyImage.Geometry
import Graphics.EasyImage.Text

axis :: Point -> Point -> Image
axis p q = arrow p q `with` [LineBlur := 0.8]

graph :: Scalar -> Scalar -> (Scalar -> Scalar) -> Image
graph x0 x1 f = g <> axis (endP unitX getX p (-w)) (endP unitX getX q w)
                  <> axis (endP unitY getY p (-h)) (endP unitY getY q h)
  where
    g = curve (\x -> Vec x (f x)) x0 x1
    Seg p q = imageBounds g
    w = getX (q - p)
    h = getY (q - p)
    endP unit get p d = unit * diag (get p + 0.1 * d)

