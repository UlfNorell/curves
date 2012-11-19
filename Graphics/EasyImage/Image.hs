{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Graphics.EasyImage.Image
  ( module Graphics.EasyImage.Image
  , (<>) )
  where

import Control.Applicative
import Data.Monoid
import Data.List
import Data.Maybe

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Attribute

type Op a = a -> a -> a

-- TODO: - explicit transformation matrices (for efficiency?)
-- | The image type.
data Image = ICurve Curve
           | Combine (Op (Maybe Colour)) Image Image
           | IEmpty

-- Image operations -------------------------------------------------------

-- | A blend function is used to compute the resulting colour when 'combine'ing
--   two images.
type BlendFunc = Maybe Colour -> Maybe Colour -> Maybe Colour

-- | Alpha 'blend' the first colour on top of the second colour.
unionBlend :: BlendFunc
unionBlend c1 c2 = case (c1, c2) of
  (Nothing, c)       -> c
  (c, Nothing)       -> c
  (Just c1, Just c2) -> Just (blend c1 c2)

-- | The alpha value of the result is the product of the alpha values of the
--   two inputs.
intersectBlend :: BlendFunc
intersectBlend c1 c2 = case (c1, c2) of
  (_, Nothing)       -> Nothing
  (Nothing, _)       -> Nothing
  (Just c1, Just c2) -> Just $ setAlpha (getAlpha c2 * getAlpha c1) (blend c1 c2)

-- | Multiplies the alpha value of the first colour by 1 - the alpha value of
--   the second colour.
diffBlend :: BlendFunc
diffBlend c c' = case c' of
  Nothing -> c
  Just c' -> opacity (1 - getAlpha c') <$> c

-- | 'mappend' = 'combine' 'unionBlend'
instance Monoid Image where
  mempty      = IEmpty
  mappend a b = Combine unionBlend a b

-- | Combine two images using the specified blend function.
combine :: BlendFunc -> Image -> Image -> Image
combine f a b = Combine f a b

infixr 7 ><
infixl 8 <->

-- | The intersection of two images.
--
-- > (><) = combine intersectBlend
(><) :: Image -> Image -> Image
a >< b = combine intersectBlend a b

-- | Subtract the second image from the first.
--
-- > (<->) = combine diffBlend
(<->) :: Image -> Image -> Image
a <-> b = combine diffBlend a b

-- | A simple curve whose points are given by the function argument. The second
--   and third arguments specify the range of the function. The function must
--   be continuous on this interval.
--
--   For example, a straight line between points @p@ and @q@ can be implemented as
--
--   @curve ('interpolate' p q) 0 1@
curve :: (Scalar -> Point) -> Scalar -> Scalar -> Image
curve f = curve' f (const id)

-- | The most general form of curve. The curve function is split in two, one
--   function from the parameter to an arbitrary 'Transformable' object, and a
--   second function from this object (and the parameter value) to a point on
--   the curve. The power of this combinator comes from the fact that
--   transformations (e.g. 'translate', 'scale', 'rotate') of the curve apply
--   only to the (result of the) first function. This means that the points
--   computed by the second function are measured in pixels of the final image.
--
--   For an example, see the 'Graphics.EasyImage.Geometry.arrow' combinator,
--   which uses a line 'Segment' as the intermediate type and computes the
--   arrow head in the second function, to ensure that the arrow head has the
--   same dimensions regardless of how the arrow is scaled.
curve' :: Transformable a => (Scalar -> a) -> (Scalar -> a -> Point) -> Scalar -> Scalar -> Image
curve' f g t0 t1 = ICurve $ Curve (f . tr) (g . tr) defaultCurveStyle
  where
    tr t = t0 + t * (t1 - t0)

mapCurves :: (Curve -> Curve) -> Image -> Image
mapCurves f IEmpty          = IEmpty
mapCurves f (ICurve c)      = ICurve (f c)
mapCurves f (Combine b i j) = Combine b (mapCurves f i) (mapCurves f j)

-- | Reverse the direction of all curves in an image. Useful in conjunction
--   with '+++'.
reverseImage :: Image -> Image
reverseImage = mapCurves reverseCurve

-- | Freeze the size of an image around the given point. Scaling the image will
--   only affect the position of the image, not the size. Translation and
--   rotation affect the image normally.
--
--   @'scaleFrom' p ('diag' k) (freezeImageSize p i) == freezeImageSize p i@
--
--   Scaling with non-uniform scale factors will still distort the image,
--   however.
freezeImageSize :: Point -> Image -> Image
freezeImageSize p = mapCurves (freezeCurve fr p)
  where
    fr = Freeze{ freezeSize = True, freezeOrientation = False }

-- | Freeze image orientation. Rotations of the image will only affect the
--   position of the image, not its orientation. Translation and scaling
--   affect the image normally.
--
--   @'rotateAround' p a (freezeImageOrientation p i) == freezeImageOrientation p i@
freezeImageOrientation :: Point -> Image -> Image
freezeImageOrientation p = mapCurves (freezeCurve fr p)
  where
    fr = Freeze{ freezeSize = False, freezeOrientation = True }

-- | Freeze both the size and the orientation of an image.
--
-- @freezeImage p i == 'freezeImageSize' p i ('freezeImageOrientation' p i)@
freezeImage :: Point -> Image -> Image
freezeImage p = mapCurves (freezeCurve fr p)
  where
    fr = Freeze{ freezeSize = True, freezeOrientation = True }

-- | Unfreeze an image. After unfreezing any frozen features will be affected
--   by transformations again.
unfreezeImage :: Image -> Image
unfreezeImage = mapCurves unfreeze
  where
    unfreeze (Curve f g s) = Curve (\t -> g t (f t)) (const id) s

instance HasAttribute CurveAttribute Image where
  modifyAttribute attr f = mapCurves (modifyAttribute attr f)

instance Transformable Image where
  transform f = mapCurves (transform f)

infixl 9 ++>
infixr 8 +++, <++

-- | Join the right-most curve of the first image to the left-most curve of the
--   second image. The 'Graphics.EasyImage.Style.Style' is inherited from the
--   curve of the first image. If the end point of the first curve does not
--   coincide with the starting point of the second curve a straight line is
--   added to connect the two. This combinator is useful when using
--   parameterized line styles (such as 'Graphics.EasyImage.Style.dashed').
(+++) :: Image -> Image -> Image
ICurve c1     +++ ICurve c2     = ICurve $ joinCurve c1 c2
i             +++ IEmpty        = i
IEmpty        +++ i             = i
Combine f i j +++ c             = Combine f i (j +++ c)
c             +++ Combine f i j = Combine f (c +++ i) j

-- | Prepend a point to the left-most curve of an image. @p <++ i@ is equivalent
--   to (but more efficient than) @'line' p q '+++' i@ if @q@ is the starting
--   point of the left-most curve of @i@.
(<++) :: Point -> Image -> Image
p <++ ICurve c      = ICurve $ prependPoint p c
p <++ Combine b i j = Combine b (p <++ i) j
p <++ IEmpty        = point p

-- | Append a point to the right-most curve of an image. @i ++> p@ is
--   equivalent to (but more efficient than) @i +++ 'line' q p@ if @q@ is the
--   end point of the right-most curve of @i@.
(++>) :: Image -> Point -> Image
ICurve c      ++> p = ICurve $ appendPoint c p
IEmpty        ++> p = point p
Combine b i j ++> p = Combine b i (j ++> p)

-- | A straight line between two points.
line :: Point -> Point -> Image
line p q = curve (interpolate p q) 0 1

-- | A single point.
point :: Point -> Image
point p = curve (const p) 0 1

-- | A circle given by its center and radius.
circle :: Point -> Scalar -> Image
circle p r = circleSegment p r 0 (2 * pi)

-- | A circle segment. The third and fourth arguments are the start and end
--   angle of the segment. If the start angle is bigger than the end angle it's
--   the clockwise segment, otherwise the counterclockwise segment. For instance,
--   @circleSegment 0 1 0 pi@ is the top half circle starting in 'unitX' and
--   ending in @-'unitX'@, whereas @circleSegment 0 1 0 (-pi)@ is the bottow
--   half circle with the same start and end points.
circleSegment :: Point -> Scalar -> Scalar -> Scalar -> Image
circleSegment c r a b | b < a = reverseImage $ circleSegment c r b a
circleSegment (Vec x y) r a b =
  curve (\α -> Vec (x + r * cos α) (y + r * sin α)) a b

-- | A connected sequence of straight lines. The list must have at least two
--   elements.
lineStrip :: [Point] -> Image
lineStrip (p:q:ps) = foldl (++>) (line p q) ps
lineStrip _ = error "lineStrip: list too short"

-- | A polygon.
--
-- > poly ps = lineStrip (ps ++ [head ps])
poly :: [Point] -> Image
poly (p:ps) = lineStrip ([p] ++ ps ++ [p])
poly [] = error "poly: []"

-- | Differentiating the curves of an image
differentiate :: Image -> Image
differentiate = mapCurves differentiateCurve

-- | Zipping two images. Both images must have the same number of curves
--   'combine'd in the same order.
zipImage :: (Scalar -> Point -> Point -> Point) -> Image -> Image -> Image
zipImage f (ICurve c) (ICurve c') = ICurve (zipCurve f c c')
zipImage f IEmpty IEmpty = IEmpty
zipImage f (Combine g a b) (Combine _ c d) =
  Combine g (zipImage f a c) (zipImage f b d)

-- B-Splines --------------------------------------------------------------

-- | A <http://en.wikipedia.org/wiki/B-spline#Uniform_cubic_B-splines uniform cubic B-spline>
--   with the given control points.
bSpline :: [Point] -> Image
bSpline ps = foldl1 (+++) $ map seg (takeWhile ((>=4).length) $ map (take 4) (tails ps))
  where
    m = map (map (/ 6)) [[-1, 3, -3, 1], [3, -6, 0, 4], [-3, 3, 3, 1], [1, 0, 0, 0]]
    coefs t = map diag $ mmul [t^3, t^2, t, 1] m
    mmul v m = map (vmul v) m
    vmul u v = sum $ zipWith (*) u v

    seg ps = curve f 0 1
      where
        f t = vmul (coefs t) ps

-- | A closed B-spline.
--
-- > closedBSpline ps = bSpline (ps ++ take 3 ps)
closedBSpline :: [Point] -> Image
closedBSpline ps = bSpline $ ps ++ take 3 ps

-- | A B-spline which starts in the first control point and ends in the last
-- control point. This is achieved by adding two extra copies of the first and
-- last points.
bSpline' (p:ps) = bSpline $ p:p:p:ps ++ replicate 2 (last (p:ps))
bSpline' [] = error "bSpline': empty list"

-- Bézier curves ----------------------------------------------------------

-- | A Bézier curve of degree n with the given control points @[p0 .. pn]@.
bezierSegment :: [Point] -> Image
bezierSegment []  = error "bezierSegment: empty list"
bezierSegment [p] = point p
bezierSegment ps  = zipImage (\t p q -> interpolate p q t) (bezierSegment (init ps)) (bezierSegment (tail ps))

-- | A strip of cubic Bézier curves.
bezier :: [Point] -> Image
bezier ps | n < 4 || mod n 3 /= 1 = error "bezier: needs 3k + 1 points (k > 0)"
  where n = length ps
bezier ps = foldr1 (+++) (map bezierSegment $ quads ps)
  where
    quads [p] = []
    quads (p0:p1:p2:p3:ps) = [p0, p1, p2, p3] : quads (p3:ps)

