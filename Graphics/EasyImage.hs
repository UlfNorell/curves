{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.EasyImage
  ( module Graphics.EasyImage.Image
  , module Graphics.EasyImage.Colour
  , module Graphics.EasyImage.Math
  , module Graphics.EasyImage.Render
  , module Graphics.EasyImage.Text
  , autoFit
  , Attr(..)
  , Basis(..)
  , lineStyle )
  where

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Curve
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Render
import Graphics.EasyImage.Compile
import Graphics.EasyImage.Text

autoFit :: Point -> Point -> Image -> Image
autoFit p q = loop
  where
    -- Repeat autoFit until reasonably stable. This makes it work for features
    -- that are scaling insensitive (line widths and frozen images).
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


