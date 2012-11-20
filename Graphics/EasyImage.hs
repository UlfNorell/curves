{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-| EasyImage provides an easy to use library for creating images. The basic
    primitive is a curve, which, in the simplest case, is a continuous function
    from a 'Scalar' parameter to a 2-dimensional 'Point' on the 'curve'. Images
    are rendered ('renderImage') as PNG images.
-}
module Graphics.EasyImage
  (
    module Graphics.EasyImage.Math
  , module Graphics.EasyImage.Colour
    -- * Image
  , Image
    -- ** Curves
  , point, line, lineStrip, poly, circle, circleSegment
    -- ** Advanced curves
  , curve, curve'
  , bSpline, bSpline', closedBSpline
  , bezier, bezierSegment
    -- ** Operating on curves
  , reverseImage
  , (+++), (+.+), (<++), (++>)
  , differentiate, mapImage, zipImage
    -- ** Advanced image manipulation
  , freezeImageSize, freezeImageOrientation, freezeImage
  , unfreezeImage
    -- ** Combining images
  , BlendFunc
  , combine, mapColour
  , unionBlend, intersectBlend, diffBlend
  , (<>)
  , (><), (<->)
    -- ** Query functions
  , imageBounds
    -- * Image attributes
    -- | Image attributes control things like the colour and width of curves.
  , module Graphics.EasyImage.Attribute
  , module Graphics.EasyImage.Style
    -- * Rendering
  , autoFit
  , renderImage
  )
  where

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Curve
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Render
import Graphics.EasyImage.Compile
import Graphics.EasyImage.Attribute
import Graphics.EasyImage.Style

-- | Scale the an image to fit inside the the box given by the two points
--   (bottom-left and top-right corners).
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

-- | Compute the bounds of an image, returning a line segment from the bottom
--   left corner to the top right corner of the bounding box. This function
--   ignores line widths. Note that using pixel based features (for instance,
--   produced by 'freezeImageSize') means that the bounds may become invalid if
--   the image is scaled.
imageBounds :: Image -> Segment
imageBounds i0
  | d < 50    = getBounds (50 / d) i
  | otherwise = s
  where
    i = i0 `with` [LineWidth := 0, LineBlur := 0, FillBlur := 0]
    s@(Seg p q) = getBounds 1 i
    d = vuncurry max (q - p)
    getBounds k' i = scale (1/k) $ bboxToSegment $ bounds $ compileImage $ scale k i
      where k = diag k'

-- ImageElement -----------------------------------------------------------

class Transformable a => ImageElement a where
  toImage :: a -> Image

instance ImageElement Image where
  toImage = id

instance ImageElement Segment where
  toImage (Seg p q) = line p q

instance ImageElement Vec where
  toImage = point

