{-# LANGUAGE MultiWayIf #-}
module Graphics.EasyImage.Style where

import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve
import Graphics.EasyImage.Attribute

modDouble a b = a - b * fromIntegral (floor (a / b))

gradient c1 c2 a =
  [VarLineColour := \d _ ->
    case modDouble d (2 * a) of
      x | x <= a    -> blend (setAlpha (x / a) c2) c1
        | otherwise -> blend (setAlpha ((2 * a - x) / a) c2) c1
  ]

dashedOpen a b =
  [VarLineColour :~ \old d r ->
    if | r == 0    -> old d r
       | otherwise ->
        let total = d/r
            n     = round (total / (a + b))
            k     = total / (fromIntegral n * (a + b) + a)
            a'    = k * a
            b'    = k * b
        in case modDouble d (a' + b') of
          x | x <= a'   -> old d r
            | otherwise -> transparent
  ]

dashedClosed a b =
  [VarLineColour :~ \old d r ->
    if | r == 0    -> old d r
       | otherwise ->
        let total = d/r
            n     = round (total / (a + b))
            k     = total / (fromIntegral n * (a + b))
            a'    = k * a
            b'    = k * b
        in case modDouble d (a' + b') of
          x | x <= a'   -> old d r
            | otherwise -> transparent
  ]

dashed a b =
  [VarLineColour :~ \old d r ->
    case modDouble d (a + b) of
      x | x <= a    -> old d r
        | otherwise -> transparent
  ]

