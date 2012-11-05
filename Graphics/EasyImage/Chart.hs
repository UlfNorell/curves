
module Graphics.EasyImage.Chart where

import Data.Monoid

import Graphics.EasyImage

barChart :: [Scalar] -> Image
barChart ys =
  arrow (Vec (-w) 0) (Vec (fullW + w) 0) <>
  arrow (Vec 0 (-w)) (Vec 0 (maximum ys + w)) <>
  mconcat [ translate (Vec (x + 0.1 * w) 0) $ scale (Vec (0.9 * w) 1) (bar y red) | (y, x) <- zip ys $ iterate (+ w) 0 ]
  where
    fullW = 1.62 * maximum ys
    w = fullW / fromIntegral (length ys)

    bar h c = rectangle 0 (Vec 1 h) `with` [FillColour := c]

