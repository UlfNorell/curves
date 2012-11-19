{-| RGBA colour values.
-}
module Graphics.EasyImage.Colour where

import Graphics.EasyImage.Math

-- | RGBA values in the range 0.0 to 1.0.
data Colour = Colour { getRed, getGreen, getBlue, getAlpha :: !Scalar }
  deriving (Eq, Ord)

instance Show Colour where
  showsPrec p (Colour r g b a) =
    showParen (p > 9) $
      showString "Colour" . foldr (\x f -> showString " " . shows x . f) id [r, g, b, a]

-- | > opacity a c = setAlpha (a * getAlpha c) c
opacity :: Scalar -> Colour -> Colour
opacity α (Colour r g b a) = Colour r g b (α * a)

-- | > opaque = setAlpha 1
opaque :: Colour -> Colour
opaque = setAlpha 1

-- | Set the alpha value of a colour.
setAlpha :: Scalar -> Colour -> Colour
setAlpha a (Colour r g b _) = Colour r g b a

-- | Check if a colour is completely transparent.
isTransparent :: Colour -> Bool
isTransparent c = 0 == round (255 * getAlpha c)

-- | @visible c == Nothing@ iff @isTransparent c@
visible :: Colour -> Maybe Colour
visible c | isTransparent c = Nothing
visible c                   = Just c

-- | Completely transparent (and black) colour.
transparent :: Colour
transparent = Colour 0 0 0 0

white, black, red, green, blue :: Colour
white = Colour 1 1 1 1
black = Colour 0 0 0 1
blue  = Colour 0 0 1 1
red   = Colour 1 0 0 1
green = Colour 0 1 0 1

-- | Alpha blending two colours.
blend :: Colour -> Colour -> Colour
blend (Colour r1 g1 b1 a1) (Colour r2 g2 b2 a2) =
  Colour (f r1 r2) (f g1 g2) (f b1 b2) (a1 + a2 * (1 - a1))
  where
    a = a1 + a2 * (1 - a1)
    f x1 x2 | a == 0    = 0
            | otherwise = (x1 * a1 + x2 * a2 * (1 - a1)) / a


