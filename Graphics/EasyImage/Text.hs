{-# LANGUAGE BangPatterns #-}
module Graphics.EasyImage.Text where

import Data.Monoid

import Graphics.EasyImage.Math
import Graphics.EasyImage.Image
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Compile
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve

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
charImage 'C' =
  bSpline [right 0.125, right 0.125, botRight, botLeft, topLeft, topRight, right 0.875, right 0.875]
  -- scaleFrom (hcenter r) (Vec 1 (1/r)) $ circleSegment (hcenter r) r (pi/4) (7/4 * pi)
  -- where
  --   r = 1 / (1 + 1 / sqrt 2)
charImage 'D' = dCup +++ line topLeft botLeft
charImage 'E' = mconcat [lineStrip [topRight, topLeft, botLeft, botRight], line (left 0.5) center]
charImage 'F' = mconcat [lineStrip [topRight, topLeft, botLeft], line (left 0.5) center]
charImage 'G' = charImage 'C' <> lineStrip [center, right 0.5, botRight]
charImage 'H' = mconcat [line topLeft botLeft, line topRight botRight, line (left 0.5) (right 0.5)]
charImage 'I' = line (top 0.5) (bot 0.5) <> line (top 0.35) (top 0.65) <> line (bot 0.35) (bot 0.65)
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
charImage 'P' = pCup +++ line topLeft botLeft
charImage 'Q' = charImage 'O' <> line (vcenter 0.25) botRight
charImage 'R' = charImage 'P' <> line (hcenter 0.5) botRight
charImage 'S' =
  bSpline [left 0.125, left 0.125, botLeft, botRight, right 0.5, left 0.5, topLeft, topRight, right 0.875, right 0.875]
-- charImage 'S' = circleSegInBox (left $ 0.5 + dx) topRight (pi/4) (3/2 * pi - α) +++
--                 mirrorH (circleSegInBox botLeft (right $ 0.5 - dx) (pi/2 + α) (7/4 * pi))
--     where α = 0.2
--           dx = 0.0027
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
charImage 'f' = circleSegInBox (coord x 0.75) (top 0.8) (pi/4) pi +++ line (coord x 0.75) (bot x) <>
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
charImage 'l' = line (top 0.5) (bot 0.5) +++ line (bot 0.5) (bot 0.65)
charImage 'm' = line botLeft (left 0.5) <> scaleFrom botLeft (Vec x 1) nHook <> translate (Vec x 0) (scaleFrom botLeft (Vec x 1) nHook)
  where x = 0.65
charImage 'n' = nHook <> line botLeft (left 0.5)
charImage 'o' = scaleFrom botLeft (Vec 1 0.5) (charImage 'O')
charImage 'p' = mirrorH (charImage 'q')
charImage 'q' = charImage 'c' <> line (right 0.5) (right (-0.25))
charImage 'r' = line botLeft (left 0.5) <> rHook
charImage 's' = scaleFrom botLeft (Vec 1 0.5) (charImage 'S')
charImage 't' = line (coord x 0.75) (coord x 0.25) +++
                circleSegInBox (bot x) (coord 0.8 0.25) pi (7/4 * pi) <>
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
charImage '6' = rotateAround center pi (charImage '9')
  -- circleSegInBox (left 0.75) topRight (pi/4) pi +++
  -- line (left 0.75) (left 0.25) +++
  -- mirrorH (circle (coord 0.5 0.25) 0.5)
charImage '7' = lineStrip [topLeft, topRight, bot 0.25]
charImage '8' = charImage 'o' <> translate (Vec 0 1) (charImage 'o')
charImage '9' = rotateAround (vcenter 0.75) (-pi/6) (circle (vcenter 0.75) 0.5) +++ line (bot 0.25) (bot 0.25)
charImage ' ' = mempty
charImage '!' = line (top 0.5) (vcenter 0.25) <> line (vcenter 0.01) (bot 0.5)
charImage '\\' = line topLeft botRight
charImage '"' = let x = 0.1; y = 0.1 in
                line (top (0.5 - x)) (coord (0.5 - x) (1 - y)) <>
                line (top (0.5 + x)) (coord (0.5 + x) (1 - y))
charImage '\'' = line (vcenter 1) (vcenter 0.9)
charImage '#' = let slant = 0.1
                    hsep  = 0.4
                    vsep  = 0.4
                in mconcat [ line (top $ 0.5 - hsep/2 + slant/2) (bot $ 0.5 - hsep/2 - slant/2)
                           , line (top $ 0.5 + hsep/2 + slant/2) (bot $ 0.5 + hsep/2 - slant/2)
                           , line (left $ 0.5 + vsep/2) (right $ 0.5 + vsep/2)
                           , line (left $ 0.5 - vsep/2) (right $ 0.5 - vsep/2) ]
charImage '$' = scaleFrom center (Vec 1 0.9) (charImage 'S') <> line (top 0.5) (bot 0.5)
charImage '%' = line botLeft topRight <> circle (coord 0.25 0.875) 0.25
                                      <> circle (coord 0.75 0.125) 0.25
charImage '+' = line (left 0.5) (right 0.5) <> line (coord 0.5 0.25) (coord 0.5 0.75)
charImage '*' = charImage '+' <> rotateAround center (pi/4) (charImage '+')
charImage '/' = line botLeft topRight
charImage '|' = line (top 0.5) (bot 0.5)
charImage '=' = line (left 0.35) (right 0.35) <> line (left 0.65) (right 0.65)
charImage '(' = circleSegInBox (bot 0.5) topRight (3/4 * pi) (5/4 * pi)
charImage ')' = mirrorH (charImage '(')
charImage '?' = line (vcenter 0.15) (vcenter 0.4) +++ scaleFrom (top 0.5) (Vec 1 1.2) (circleSegment (vcenter 0.75) 0.5 (-pi/2) (3/4 * pi)) <>
                line (vcenter 0) (vcenter 0.01)
charImage '-' = line (left 0.5) (right 0.5)
charImage '_' = line botLeft botRight
charImage '^' = lineStrip [coord 0.25 0.75, top 0.5, coord 0.75 0.75]
charImage '`' = line (top 0.45) (coord 0.55 0.9)
charImage '~' = cap +++ translate (Vec 0.5 (-y * 2)) (mirrorV cap)
  where
    y = 1/30
    h = 0.1
    cap = scaleFrom (bot 0.25) (Vec (-1) 1) $ circleSegInBox (left $ 0.5 - h/2 + y) (vcenter $ 0.5 + h/2 + y) (pi/4) (3/4 * pi)
charImage '[' = lineStrip [topRight, top 0.5, bot 0.5, botRight]
charImage ']' = mirrorH (charImage '[')
charImage '{' = half +++ reverseImage (mirrorV half)
  where
    half = circleSegInBox (vcenter 0.75) topRight (pi/2) pi +++
           line (vcenter 0.75) (vcenter 0.65) +++
           reverseImage (circleSegInBox (hcenter 0.3) (vcenter 0.65) (3/2 * pi) (2 * pi))
charImage '}' = mirrorH $ charImage '{'
charImage '.' = line (bot 0.5) (vcenter 0.01)
charImage ',' = line (bot 0.55) (coord 0.45 (-0.1))
charImage ';' = charImage ',' <> line center (vcenter 0.49)
charImage ':' = charImage '.' <> line center (vcenter 0.49)
charImage '<' = lineStrip [right 0.75, left 0.5, right 0.25]
charImage '>' = mirrorH $ charImage '<'
charImage '@' = f (charImage 'c') <>
                f (line (right 0.5) botRight) +++
                circleSegInBox botLeft (right 0.75) (-pi/4) (3/2 * pi)
  where
    f = translate (Vec 0 0.3) . scaleFrom (bot 0.5) 0.7
charImage '&' =
  bSpline [right 0.5, right 0.25, bot 0.75, botLeft, left 0.4, coord 0.85 0.75, top 0.85, top 0.15, coord 0.15 0.75, botRight, botRight, botRight]
-- charImage '&' = reverseImage (circleSegInBox botLeft (right 0.5) (3/4 * pi) (2 * pi)) +++
--                 circleSegInBox (coord x 0.75) (top (x + w)) (-pi/4) (5/4 * pi - 0.28) +++ line botRight botRight
--   where
--     w = 0.7
--     x = 0.1
charImage 'Α' = charImage 'A'
charImage 'Β' = charImage 'B'
charImage 'Γ' = lineStrip [topRight, topLeft, botLeft]
charImage 'Δ' = poly [botLeft, top 0.5, botRight]
charImage 'Ε' = charImage 'E'
charImage 'Ζ' = charImage 'Z'
charImage 'Η' = charImage 'H'
charImage 'Θ' = scaleFrom center (Vec 1.2 2) (circle center 0.5) <> line (coord 0.1 0.5) (coord 0.9 0.5)
charImage 'Ι' = charImage 'I'
charImage 'Κ' = charImage 'K'
charImage 'Λ' = lineStrip [botLeft, top 0.5, botRight]
charImage 'Μ' = charImage 'M'
charImage 'Ν' = charImage 'N'
charImage 'Ξ' = mconcat [line topLeft topRight, line (hcenter 0.2) (hcenter 0.8), line botLeft botRight]
charImage 'Ο' = charImage 'O'
charImage 'Π' = mconcat [line (top (-0.1)) (top 1.1), line (top 0.1) (bot 0.1), line (top 0.9) (bot 0.9)]
charImage 'Ρ' = charImage 'P'
charImage 'Σ' = lineStrip [topRight, topLeft, center, botLeft, botRight]
charImage 'Τ' = charImage 'T'
charImage 'Υ' = charImage 'Y'
charImage 'Φ' = circle center 0.75 <> line (top 0.5) (bot 0.5)
charImage 'Χ' = charImage 'X'
charImage 'Ψ' = bSpline' [topLeft, left 0.4, right 0.4, topRight] <> line (top 0.5) (bot 0.5)
charImage 'Ω' = botLeft <++ bSpline' [bot 0.3, left 0.3, topLeft, topRight, right 0.3, bot 0.7] ++> botRight
charImage 'α' = bSpline' [right 0.5, bot 0.6, botLeft, left 0.5, hcenter 0.6, bot 0.8, botRight]
charImage 'β' = bSpline' [left (-0.15), left 0.2, left 0.8, r 0.8, r 0.5, l 0.4, l 0.4, r' 0.45, r' (-0.05), botLeft]
  where r = coord 0.8
        r' = coord 0.85
        l = coord 0.1
charImage 'γ' = bSpline' [left 0.4, hcenter 0.1, bot 0.4, coord 0.55 (-0.25), coord 0.35 (-0.25), bot 0.4, r 0.5]
  where r = coord 0.8
charImage 'δ' = bSpline' [coord 0.8 0.9, top 0.5, top 0.1, coord 0.1 0.7, right 0.5, botRight, botLeft, left 0.5, coord 0.58 0.58]
charImage 'ε' = bSpline' [r 0.45, hcenter 0.6, left 0.5, left 0.25, coord 0.4 0.25, coord 0.4 0.25, left 0.25, botLeft, bot 0.6, r 0.05]
  where r = coord 0.7
charImage 'ζ' = bSpline' [left 0.8, right 0.8, topRight, left 0.8, left (-0.1), botRight, right (-0.25), coord 0.25 (-0.25)]
charImage 'η' = right (-0.25) <++ charImage 'n'
charImage 'θ' = scaleFrom center (Vec 1 (1/r)) (circle center r) <> line (hcenter (0.5 - r)) (hcenter (0.5 + r))
  where r = 0.4
charImage 'ι' = bSpline' [center, coord 0.5 (-0.1), coord 0.75 0.1]
charImage 'κ' = scaleFrom botLeft (Vec 0.8 0.5) (charImage 'K') -- mconcat [line botLeft (left 0.5), line (left 0.25) (hcenter 0.8), line (left 0.25) (bot 0.8)]
charImage 'λ' = mconcat [line topLeft botRight, line center botLeft]
charImage 'μ' = left (-0.25) <++ charImage 'u'
charImage 'ν' = left 0.5 <++ bSpline' [bot 0.4, coord 0.8 0.25, coord 0.8 0.5]
charImage 'ξ' = bSpline' [left 0.7, right 0.7, right 0.9, left 0.7, left 0.35, coord 0.8 0.3, coord 0.8 0.4, left 0.35, left (-0.1), botRight, right (-0.25), coord 0.25 (-0.25)] 
charImage 'ο' = charImage 'o'
charImage 'π' = mconcat [line (bot r) (hcenter r), line (left 0.5) (right 0.5), bSpline' [hcenter (1 - r), coord (1 - r) r, botRight]]
  where r = 0.2
charImage 'ρ' = bSpline' [coord 0.1 0.1, bot 0.5, botRight, right 0.5, left 0.5, left (-0.1), coord 0.8 (-0.1), coord 0.8 (-0.25), coord 0.25 (-0.25)]
charImage 'σ' = right 0.5 <++ scaleFrom center (Vec 0.8 1) (rotateAround (vcenter 0.25) (pi/2) (charImage 'o'))
charImage 'τ' = charImage 'ι' <> line (left 0.5) (right 0.5)
charImage 'υ' = bSpline' [left 0.5, botLeft, bot 0.5, coord 0.8 0.25, coord 0.8 0.5]
charImage 'φ' = bSpline' [hcenter 0.4, left 0.4, botLeft, botRight, right 0.5, hcenter 0.4, vcenter (-0.25)]
charImage 'χ' = line (left 0.5) (r (-0.25)) <> line (r 0.5) (left (-0.25))
  where r = coord 0.8
charImage 'ψ' = bSpline' [hcenter 0.1, botLeft, botRight, coord 0.9 0.5] <> line (vcenter 0.6) (vcenter (-0.25))
charImage 'ω' = bSpline' [hcenter 0.1, l 0.35, botLeft, bot 0.5, coord 0.55 0.4, coord 0.45 0.4, bot 0.5, botRight, r 0.35, hcenter 0.9]
  where l = coord (-0.1)
        r = coord 1.1
charImage _   = poly [0, Vec 1 0, Vec 1 2, Vec 0 2]

charPos ' ' = (0, 0.3)
charPos c = (getX p / k, getX (q - p) / k)
  where
    k = 10
    i = scale (diag k) (charImage c)
    Seg p q = bboxToSegment $ bounds $ compileImage i

data Alignment = LeftAlign | RightAlign | CenterAlign

stringImage' :: Alignment -> Scalar -> String -> Image
stringImage' _ _ "" = mempty
stringImage' align spacing s =
  case align of
    LeftAlign   -> i
    RightAlign  -> translate (Vec (-w) 0) i
    CenterAlign -> translate (Vec (-w/2) 0) i
  where
    (i, w) = render 0 s
    render w  []    = (mempty, w - spacing)
    render !x (c:s) = (translate (Vec (x - dx) 0) (charImage c) <> i, w')
      where
        (dx, w) = charPos c
        (i, w') = render (x + w + spacing) s

stringImage = stringImage' LeftAlign 0.1

label :: Point -> Scalar -> String -> Image
label p h s = translate p $ freezeImage 0 $ scale (diag $ h/2) $ translate (Vec 0 (-1)) $ stringImage' CenterAlign 0.1 s

