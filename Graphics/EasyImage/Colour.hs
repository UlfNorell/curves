
module Graphics.EasyImage.Colour where

import Codec.Picture
import Graphics.EasyImage.Math

-- | RGBA (range 0.0 to 1.0)
data Colour = Colour !Scalar !Scalar !Scalar !Scalar
  deriving (Show, Eq, Ord)

transparency :: Scalar -> Colour -> Colour
transparency α (Colour r g b a) = Colour r g b (α * a)

transparent :: Colour
transparent = Colour 0 0 0 0

opaque :: Colour -> Colour
opaque (Colour r g b _) = Colour r g b 1

getAlpha :: Colour -> Scalar
getAlpha (Colour _ _ _ a) = a

setAlpha :: Scalar -> Colour -> Colour
setAlpha a (Colour r g b _) = Colour r g b a

isZero :: Scalar -> Bool
isZero x = 0 == round (255 * x)

visible :: Colour -> Maybe Colour
visible c | isZero (getAlpha c) = Nothing
visible c                       = Just c

white = Colour 1 1 1 1
black = Colour 0 0 0 1
blue  = Colour 0 0 1 1
red   = Colour 1 0 0 1
green = Colour 0 1 0 1

toRGBA :: Colour -> PixelRGBA8
toRGBA (Colour r g b a) = PixelRGBA8 (f r) (f g) (f b) (f a)
  where
    f x = round (255 * x)

type BlendFunc = Colour -> Colour -> Colour

blend :: BlendFunc
blend (Colour r1 g1 b1 a1) (Colour r2 g2 b2 a2) =
  Colour (f r1 r2) (f g1 g2) (f b1 b2) (a1 + a2 * (1 - a1))
  where
    f x1 x2 = x1 * a1 + x2 * a2 * (1 - a1)


