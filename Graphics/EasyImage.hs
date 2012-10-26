{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.EasyImage
  ( module Graphics.EasyImage.Image
  , module Graphics.EasyImage.Colour
  , module Graphics.EasyImage.Math
  , module Graphics.EasyImage.Render
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

