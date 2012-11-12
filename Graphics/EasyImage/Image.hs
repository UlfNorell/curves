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
data Image = ICurve Curve
           | Combine (Op (Maybe Colour)) Image Image
           | IEmpty

-- Image operations -------------------------------------------------------

type BlendFunc = Maybe Colour -> Maybe Colour -> Maybe Colour

unionBlend :: BlendFunc
unionBlend c Nothing = c
unionBlend Nothing c = c
unionBlend (Just c1) (Just c2) = visible $ blend c1 c2

intersectBlend :: BlendFunc
intersectBlend c Nothing = Nothing
intersectBlend Nothing c = Nothing
intersectBlend (Just c1) (Just c2) = visible $ setAlpha (getAlpha c2 * getAlpha c1) (blend c1 c2)

diffBlend :: BlendFunc
diffBlend c (Just c') = visible . opacity (1 - getAlpha c') =<< c
diffBlend c Nothing   = c

instance Monoid Image where
  mempty      = IEmpty
  mappend a b = Combine unionBlend a b

combine :: BlendFunc -> Image -> Image -> Image
combine f a b = Combine f a b

infixr 6 ><
infixl 7 <->

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

curve :: (Scalar -> Point) -> Scalar -> Scalar -> Image
curve f = curve' f (const id)

curve' :: Transformable a => (Scalar -> a) -> (Scalar -> a -> Point) -> Scalar -> Scalar -> Image
curve' f g t0 t1 = ICurve $ Curve f g t0 t1 defaultCurveStyle

mapCurves :: (Curve -> Curve) -> Image -> Image
mapCurves f IEmpty          = IEmpty
mapCurves f (ICurve c)      = ICurve (f c)
mapCurves f (Combine b i j) = Combine b (mapCurves f i) (mapCurves f j)

reverseImage :: Image -> Image
reverseImage = mapCurves reverseCurve

-- | Freeze the size of an image.
freezeImageSize :: Point -> Image -> Image
freezeImageSize p = mapCurves (freezeCurve fr p)
  where
    fr = Freeze{ freezeSize = True, freezeOrientation = False }

-- | Freeze image orientation
freezeImageOrientation :: Point -> Image -> Image
freezeImageOrientation p = mapCurves (freezeCurve fr p)
  where
    fr = Freeze{ freezeSize = False, freezeOrientation = True }

-- | Freeze both size and orientation
freezeImage :: Point -> Image -> Image
freezeImage p = mapCurves (freezeCurve fr p)
  where
    fr = Freeze{ freezeSize = True, freezeOrientation = True }

instance HasAttribute CurveAttribute Image where
  modifyAttribute attr f = mapCurves (modifyAttribute attr f)

instance Transformable Image where
  transform f = mapCurves (transform f)

infixl 8 ++>
infixr 7 +++, <++
(+++) :: Image -> Image -> Image
ICurve c1 +++ ICurve c2 = ICurve $ joinCurve c1 c2
i +++ IEmpty = i
IEmpty +++ i = i
Combine f i j +++ c = Combine f i (j +++ c)
c +++ Combine f i j = Combine f (c +++ i) j

(<++) :: Point -> Image -> Image
p <++ ICurve c      = ICurve $ prependPoint p c
p <++ Combine b i j = Combine b (p <++ i) j
_ <++ IEmpty        = error "_ <++ mempty"

(++>) :: Image -> Point -> Image
ICurve c      ++> p = ICurve $ appendPoint c p
IEmpty        ++> _ = error "mempty ++> _"
Combine b i j ++> p = Combine b i (j ++> p)

line :: Point -> Point -> Image
line p q = curve (interpolate p q) 0 1

point :: Point -> Image
point p = curve (const p) 0 1

circle :: Point -> Scalar -> Image
circle p r = circleSegment p r 0 (2 * pi)

circleSegment :: Point -> Scalar -> Scalar -> Scalar -> Image
circleSegment (Vec x y) r a b =
  curve (\α -> Vec (x + r * cos α) (y + r * sin α)) a b

lineStrip :: [Point] -> Image
lineStrip (p:q:ps) = foldl (++>) (line p q) ps
lineStrip _ = error "lineStrip: list too short"

poly :: [Point] -> Image
poly (p:ps) = lineStrip ([p] ++ ps ++ [p])
poly [] = error "poly: []"

-- B-Splines --------------------------------------------------------------

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

closedBSpline :: [Point] -> Image
closedBSpline ps = bSpline $ ps ++ take 3 ps

bSpline' (p:ps) = bSpline $ p:p:p:ps ++ replicate 2 (last (p:ps))
bSpline' [] = error "bSpline': empty list"

