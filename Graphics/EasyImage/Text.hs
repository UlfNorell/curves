
module Graphics.EasyImage.Text where

import Data.Monoid

import Graphics.EasyImage.Math
import Graphics.EasyImage.Image

center   = Vec 0.5 1
topLeft  = Vec 0 2
topRight = Vec 1 2
botLeft  = Vec 0 0
botRight = Vec 1 0
left = interpolate botLeft topLeft
right = interpolate botRight topRight
top = interpolate topLeft topRight
bot = interpolate botLeft botRight
hcenter = interpolate (left 0.5) (right 0.5)
vcenter = interpolate (bot 0.5) (top 0.5)

coord x y = botLeft + diag x * (botRight - botLeft) + diag y * (topLeft - botLeft)

dCup' t b =
  foldr1 (+++) $
    [ line botLeft (bot d) | b ] ++
    [ scaleFrom center (Vec 1 (1/(1 - d))) $ circleSegment (hcenter d) (1 - d) (-pi/2) (pi/2) ] ++
    [ line (top d) topLeft | t ]
  where
    d = 0.5

circleSegInBox p0 p1 α β =
  translate (p0 - q0) $ scaleFrom q0 d $ circleSegment 0 1 α β
  where
    included x = x >= α && x <= β
    foo f h γ | included γ = f γ
              | otherwise  = h (f α) (f β)
    y1 = foo sin max (pi/2)
    y0 = foo sin min (3/2 * pi)
    x1 = foo cos max 0
    x0 = foo cos min pi
    q0 = Vec x0 y0
    q1 = Vec x1 y1
    d  = (p1 - p0) / (q1 - q0)

dCup = dCup' True True
pCup = scaleFrom topLeft (Vec 1 0.5) dCup

mirrorH i = scaleFrom center (Vec (-1) 1) i
mirrorV i = scaleFrom center (Vec 1 (-1)) i

jHook = line (right 0.5) (right (-0.125)) +++
        mirrorH (circleSegInBox (left (-0.25)) (right (-0.125)) pi (2*pi))
nHook = line botRight (right 0.25) +++ rHook
rHook = circleSegInBox (left 0.25) (right 0.5) 0 pi

charImage :: Char -> Image
charImage 'A' = lineStrip [botLeft, top 0.5, botRight] <> line (coord 0.2 0.4) (coord 0.8 0.4)
charImage 'B' = scaleFrom topLeft (Vec 0.9 1) (charImage 'P') <> scaleFrom botLeft (Vec 1 0.5) (dCup' False True)
charImage 'C' = scaleFrom (hcenter r) (Vec 1 (1/r)) $ circleSegment (hcenter r) r (pi/4) (7/4 * pi)
  where
    r = 1 / (1 + 1 / sqrt 2)
charImage 'D' = dCup <> line topLeft botLeft
charImage 'E' = mconcat [lineStrip [topRight, topLeft, botLeft, botRight], line (left 0.5) center]
charImage 'F' = mconcat [lineStrip [topRight, topLeft, botLeft], line (left 0.5) center]
charImage 'G' = charImage 'C' <> lineStrip [center, right 0.5, botRight]
charImage 'H' = mconcat [line topLeft botLeft, line topRight botRight, line (left 0.5) (right 0.5)]
charImage 'I' = line (top 0.5) (bot 0.5)
charImage 'J' = circleSegment (coord 0.5 0.25) 0.5 pi (2 * pi) +++
                lineStrip [right 0.25, topRight, top 0.5]
charImage 'K' = mconcat [line topLeft botLeft, line (left y) topRight, line (coord x (y + x * (1 - y))) botRight ]
  where
    x = 0.3
    y = 0.3
charImage 'L' = lineStrip [topLeft, botLeft, botRight]
charImage 'M' = lineStrip [botLeft, topLeft, vcenter 0.25, topRight, botRight]
charImage 'N' = lineStrip [botLeft, topLeft, botRight, topRight]
charImage 'O' = scaleFrom center (Vec 1 2) (circle center 0.5)
charImage 'P' = pCup <> line topLeft botLeft
charImage 'Q' = charImage 'O' <> line (vcenter 0.25) botRight
charImage 'R' = charImage 'P' <> line (hcenter 0.5) botRight
charImage 'S' = circleSegInBox (left $ 0.5 + dx) topRight (pi/4) (3/2 * pi - α) +++
                mirrorH (circleSegInBox botLeft (right $ 0.5 - dx) (pi/2 + α) (7/4 * pi))
    where α = 0.2
          dx = 0.0027
charImage 'T' = line topLeft topRight <> line (top 0.5) (bot 0.5)
charImage 'U' = line topLeft (left 0.25) +++
                circleSegment (coord 0.5 0.25) 0.5 pi (2 * pi) +++
                line (right 0.25) topRight
charImage 'V' = lineStrip [topLeft, bot 0.5, topRight]
charImage 'W' = lineStrip [topLeft, bot 0.25, center, bot 0.75, topRight]
charImage 'X' = line topLeft botRight <> line botLeft topRight
charImage 'Y' = line (bot 0.5) center <> lineStrip [topLeft, center, topRight]
charImage 'Z' = lineStrip [topLeft, topRight, botLeft, botRight]
charImage 'a' = charImage 'c' <> line botRight (right 0.5)
charImage 'b' = mirrorH (charImage 'c') <> line topLeft botLeft
charImage 'c' = circleSegInBox botLeft (right 0.5) (pi/4) (7/4 * pi)
charImage 'd' = charImage 'c' <> line botRight topRight
charImage 'e' = line (left 0.25) (right 0.25) +++ circleSegInBox botLeft (right 0.5) 0 (7/4 * pi)
charImage 'f' = circleSegInBox (coord x 0.75) topRight (pi/4) pi +++ line (coord x 0.75) (bot x) <>
                line (left 0.5) (hcenter (2 * x))
  where x = 0.3
charImage 'g' = charImage 'c' <> jHook
charImage 'h' = line botLeft topLeft <> nHook
charImage 'i' = line center (bot 0.5) <> line (vcenter 0.65) (vcenter 0.64)
charImage 'j' = line center (right 0.5) +++ jHook <> line (right 0.65) (right 0.64)
charImage 'k' = mconcat [line topLeft botLeft, line (left y) (right 0.5), line (coord x (y + x * (0.5 - y))) botRight ]
  where
    x = 0.3
    y = 0.2
charImage 'l' = line (top 0.5) (bot 0.5)
charImage 'm' = line botLeft (left 0.5) <> scaleFrom botLeft (Vec x 1) nHook <> translate (Vec x 0) (scaleFrom botLeft (Vec x 1) nHook)
  where x = 0.65
charImage 'n' = line botLeft (left 0.5) <> nHook
charImage 'o' = scaleFrom botLeft (Vec 1 0.5) (charImage 'O')
charImage 'p' = mirrorH (charImage 'q')
charImage 'q' = charImage 'c' <> line (right 0.5) (right (-0.25))
charImage 'r' = line botLeft (left 0.5) <> rHook
charImage 's' = scaleFrom botLeft (Vec 1 0.5) (charImage 'S')
charImage 't' = line (coord x 0.75) (coord x 0.25) +++
                circleSegInBox (bot x) (right 0.25) pi (7/4 * pi) <>
                line (left y) (coord (2 * x) y)
  where x = 0.3; y = 0.4
charImage 'u' = rotateAround (vcenter 0.25) pi (charImage 'n')
charImage 'v' = scaleFrom botLeft (Vec 1 0.5) (charImage 'V')
charImage 'w' = scaleFrom botLeft (Vec 1 0.5) (charImage 'W')
charImage 'x' = scaleFrom botLeft (Vec 1 0.5) (charImage 'X')
charImage 'y' = rotateAround (vcenter 0.25) pi nHook <> jHook
charImage 'z' = scaleFrom botLeft (Vec 1 0.5) (charImage 'Z')
charImage '0' = charImage 'O' <> line (coord (0.5 - r) (0.5 - r)) (coord (0.5 + r) (0.5 + r))
  where r = 0.5 / sqrt 2
charImage '1' = lineStrip [coord 0.25 0.75, top 0.5, bot 0.5]
charImage '2' = mirrorH (circleSegInBox (left 0.75) topRight 0 (5/4 * pi - 0.4)) +++
                line botLeft botRight
  where r = 0.5 / sqrt 2
charImage '3' = mirrorH (circleSegInBox (left 0.5) topRight (pi/6) (3/2 * pi)) +++
                mirrorH (circleSegInBox botLeft (right 0.5) (pi/2) (11/6 * pi))
charImage '4' = lineStrip [bot 0.7, top 0.7, left 0.4, right 0.4]
charImage '5' = lineStrip [topRight, topLeft, left 0.6] +++
                mirrorH (circleSegInBox botLeft (right 0.6) (pi/2) (7/4 * pi))
charImage '6' = circleSegInBox (left 0.75) topRight (pi/4) pi +++
                line (left 0.75) (left 0.25) +++
                mirrorH (circle (coord 0.5 0.25) 0.5)
charImage '7' = lineStrip [topLeft, topRight, bot 0.25]
charImage '8' = charImage 'o' <> translate (Vec 0 1) (charImage 'o')
charImage '9' = rotateAround (vcenter 0.75) (-pi/6) (circle (vcenter 0.75) 0.5) +++ line (bot 0.25) (bot 0.25)
charImage ' ' = mempty
charImage _   = poly [0, Vec 1 0, Vec 1 2, Vec 0 2]

stringImage :: String -> Image
stringImage s = mconcat $ zipWith f s (iterate (+1.3) 0)
  where
    f c x = translate (Vec x 0) $ charImage c
