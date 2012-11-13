{-# LANGUAGE MultiWayIf #-}
module Graphics.EasyImage.Style
  ( CurveAttribute(..)
  , Style
  , lineStyle, fillStyle
  , gradient, dashedOpen, dashedClosed, dashed
  ) where

import Graphics.EasyImage.Math
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve
import Graphics.EasyImage.Attribute

-- | A style is a list of attribute assignments.
type Style = [Assignment Image]

-- | Setting the line width, blur and colour.
lineStyle :: Scalar -> Scalar -> Colour -> Style
lineStyle w b c = [LineWidth := w, LineBlur := b, LineColour := c]

-- | Setting the fill blur and colour.
fillStyle :: Scalar -> Colour -> Style
fillStyle b c   = [FillColour := c, FillBlur := b]

modDouble a b = a - b * fromIntegral (floor (a / b))

-- | Fade from the first to the second colour and then back. The third argument
-- in the distance in pixels it takes to reach the second colour.
gradient :: Colour -> Colour -> Scalar -> Style
gradient c1 c2 a =
  [VarLineColour := \d _ ->
    case modDouble d (2 * a) of
      x | x <= a    -> blend (setAlpha (x / a) c2) c1
        | otherwise -> blend (setAlpha ((2 * a - x) / a) c2) c1
  ]

-- | A dashed line style. The first argument is the approximate length (in
--   pixels) of the dashes and the second argument of the gaps. The lengths are
--   adjusted to make the curve always end in a dash.
dashedOpen :: Scalar -> Scalar -> Style
dashedOpen a b =
  dashed a b ++
  [VarLineColour :~ \old d r ->
    if | r == 0    -> old d r
       | otherwise ->
        let total = d/r
            n     = round (total / (a + b))
            k     = total / (fromIntegral n * (a + b) + a)
        in old (d / k) r
  ]

-- | A dashed line style. The first argument is the approximate length (in
--   pixels) of the dashes and the second argument of the gaps. The lengths are
--   adjusted to make the curve always end in a gap so closed curves have a
--   smooth transition where the end of the curve meets the start.
dashedClosed :: Scalar -> Scalar -> Style
dashedClosed a b =
  dashed a b ++
  [VarLineColour :~ \old d r ->
    if | r == 0    -> old d r
       | otherwise ->
        let total = d/r
            n     = round (total / (a + b))
            k     = total / (fromIntegral n * (a + b))
        in old (d / k) r
  ]

-- | A dashed line style. The first argument is the lengths (in pixels) of the
--   dashes and the second argument of the gaps.
dashed :: Scalar -> Scalar -> Style
dashed a b =
  [VarLineColour :~ \old d r ->
    case modDouble d (a + b) of
      x | x <= a    -> old d r
        | otherwise -> transparent
  ]

