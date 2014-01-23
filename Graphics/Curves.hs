{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
{-| Curves is an easy to use library for creating images. The basic
    primitive is a curve, which, in the simplest case, is a continuous function
    from a 'Scalar' parameter to a 2-dimensional 'Point' on the 'curve'. Images
    are rendered ('renderImage') as PNG images.
-}
module Graphics.Curves
  (
    module Graphics.Curves.Math
  , module Graphics.Curves.Colour
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
  , differentiate, mapImage, zipImage, transformImage
  , curveLength
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
  , module Graphics.Curves.Attribute
  , module Graphics.Curves.Style
    -- * Rendering
  , autoFit, autoStretch
  , renderImage
  )
  where

import Graphics.Curves.Math
import Graphics.Curves.BoundingBox
import Graphics.Curves.Curve
import Graphics.Curves.Image
import Graphics.Curves.Colour
import Graphics.Curves.Render
import Graphics.Curves.Compile
import Graphics.Curves.Attribute
import Graphics.Curves.Style

-- | Scale the an image to fit inside the the box given by the two points
--   (bottom-left and top-right corners).
autoFit :: Point -> Point -> Image -> Image
autoFit p q = loop 0
  where
    -- Repeat autoFit until reasonably stable. This makes it work for features
    -- that are scaling insensitive (line widths and frozen images).
    loop oldk i
      | abs (k - 1) < 0.01    = i'
      | abs (oldk - k) < 0.01 = i'  -- not making progress
      | otherwise             = loop k i'
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

-- | Scale the an image to fit inside the the box given by the two points
--   (bottom-left and top-right corners). Does not preserve aspect ratio.
autoStretch :: Point -> Point -> Image -> Image
autoStretch p q = loop 0
  where
    -- Repeat autoStretch until reasonably stable. This makes it work for features
    -- that are scaling insensitive (line widths and frozen images).
    loop oldk i
      | abs (getX k - 1) < 0.01 &&
        abs (getY k - 1) < 0.01      = i'
      | getX (abs $ oldk - k) < 0.01 = i'  -- not making progress
      | otherwise                    = loop k i'
      where
        (k, i') = autoStretch' p q i

autoStretch' :: Point -> Point -> Image -> (Vec, Image)
autoStretch' p0 p1 i =
  (k, translate (p0 - q0 + offs) $ scaleFrom q0 k i)
  where
    Seg q0 q1 = bboxToSegment $ bounds $ compileImage i
    screen = p1 - p0
    world  = q1 - q0
    k      = screen / world
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

