
module Graphics.EasyImage.Graph where

import Graphics.EasyImage

graph :: Scalar -> Scalar -> (Scalar -> Scalar) -> Image
graph x0 x1 f = curve (\x -> Vec x (f x)) x0 x1

