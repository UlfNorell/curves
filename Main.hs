{-# LANGUAGE MultiWayIf #-}
import System.Environment

import Data.List
import Data.Monoid

import Graphics.EasyImage
import Debug.Trace

circle' :: Point -> Scalar -> Image
circle' (Vec x y) r =
  curve (\α -> Vec (x + r * cos α) (y + r * sin α)) 0 (2 * pi) `with` lineStyle 1 1.2 red

ellipse :: Point -> Scalar -> Scalar -> Image
ellipse (Vec x y) r d = curve f 0 (2 * pi) `with` lineStyle 3 5 (transparency 0.7 blue)
  where
    f α = Vec (x + (r - s) * cos α) (y + (r - s) * sin α)
      where
        s = (d^2 + r^2 - 2 * d * r * cos α) / (2 * r - 2 * d * cos α)

graph :: Scalar -> Scalar -> (Scalar -> Scalar) -> Image
graph x0 x1 f = curve (\x -> Vec x (f x)) x0 x1

save i = renderImage "test.png" 800 600 white i

outline i = (i `with` [ LineWidth 3, LineColour white ]) <>
            (i `with` [ LineWidth 5, LineColour $ Colour 0.8 0 0.6 1 ])

main =
  save $ autoFit (Vec 20 20) (Vec 780 580) $
    -- circle 0 3 `with` [ LineWidth 1, LineBlur 20, LineColour red ] <>
    -- poly [-1, Vec 1 (-1), Vec 0 1]
    -- image1
    -- circle (Vec 0 0) 10 `with` [ LineWidth 1, LineBlur 10, LineColour $ transparency 1 red
    --                            , FillColour $ transparency 0.5 blue ] <>
    -- circle (Vec 12 0) 8 `with` [ LineColour transparent, FillColour $ Colour 0.1 0.4 0 1 ]
    -- poly [100, Vec 300 100, 300, Vec 100 300] <>
    -- (scaleFrom center (Vec 10 2) $ rotateAround center (pi/3) $ arrow (Vec 370 300) center)
    --   `with` [LineWidth 1]
    -- <> circle center 20 `with` [FillColour $ Colour 1 0.7 0.7 1]
    -- <> scaleFrom 100 2 (rotateAround 100 (pi/3) angleTest)
    -- arrow 1 (Vec 3 2) <>
    -- arrow 1 (Vec 3 1) <>
    -- angleArc 1 (Vec 3 2) (Vec 3 1) <>
    -- circle (Vec 2 2) 3 `with` (gradient red blue 100 ++ [LineWidth 10, LineBlur 5]) -- , FillColour (Colour 0 0 1 0.4), FillBlur 15])
    -- rotate (pi/4) (freezeImage 0 $ scale 20 $ text "Hello World!") `with` [LineWidth 1] <>
    -- text (unlines $ chunks 18 $ delete '\x3a2' ['Α'..'Ω'] ++ delete 'ς' ['α'..'ω'])
    --  `with` [LineWidth 1]
    mconcat [ rotate a $ translate (Vec 100 0) $ freeze 0 $ scale 15 $ stringImage [c]
            | (freeze, (c, a)) <- zip (cycle [freezeImage, freezeImageSize, freezeImageOrientation, const id]) $ angled ['A'..'Z'] ] <>
    (circle 0 10 <> circle 0 11)`with` [LineColour $ Colour 0.7 0.7 0.7 1]
  where
    angled xs = zip xs (iterate (\a -> a + 2 * pi / n) 0)
      where n = fromIntegral (length xs)
    text s = mconcat $ zipWith f (iterate (subtract 2.7) 0) (lines s)
      where f y s = translate (Vec 0 y) (stringImage' 0 s)
    angleTest =
      line p1 p0 +++ line p0 p2 <> angleArc p0 p1 p2
      where
        p0 = Vec 100 100
        p1 = Vec 150 120
        p2 = Vec 130 150
    center = Vec 400 300

chunks n [] = []
chunks n xs = ys : chunks n zs
  where
    (ys, zs) = splitAt n xs

angleArc :: Point -> Point -> Point -> Image
angleArc p0 p1 p2 = curve' f g 0 1
  where
    f _ = (p0, p1, p2)
    g t (p0, p1, p2) = p0 + 20 * rot (t * α) (norm (p1 - p0))
      where
        α = angle (p1 - p0) (p2 - p0)

arrow :: Point -> Point -> Image
arrow from to = curve' f g 0 2 <> line from to `with` dashed black 10 10
  where
    f = const $ Seg from to
    g t (Seg from to) =
        if | t <= 1    -> interpolate fin1 to t
           | otherwise -> interpolate to fin2 (t - 1)
      where
        v    = norm (from - to)
        fin1 = to + 20 * rot (pi/6) v
        fin2 = to + 20 * rot (-pi/6) v

modDouble a b = a - b * fromIntegral (floor (a / b))

gradient c1 c2 a =
  [VarLineColour $ \d ->
    case modDouble d (2 * a) of
      x | x <= a    -> blend (setAlpha (x / a) c2) c1
        | otherwise -> blend (setAlpha ((2 * a - x) / a) c2) c1
  ]

dashed c a b =
  [VarLineColour $ \d ->
    case modDouble d (a + b) of
      x | x <= a    -> c
        | otherwise -> transparent
  ]

image1 =
    rotateAround (Vec 400 325) (pi/6) (ellipse (Vec 400 300) 400 350) <>
    translate (Vec 250 300) (scale 200 $ circle (Vec 0 0) 1) <> point (Vec 250 300) <>
    outline (ellipse (Vec 200 200) 200 120) <>
    (translate (Vec 20 150) $ scale (Vec 30 100) $ graph 0 (8 * pi) f `with`
      [ LineWidth 2, LineColour $ Colour 0 0.5 0.2 1
      , FillColour $ transparency 0.3 green ])
  where
    f x = sin x + 0.5 * sin (3 * x)

-- TODO
--    * text
--      - auto kerning (how?)
--    * parameterize width and blur as well (allow calligraphy style curves)
--        - still need a max width for bounding box calculation
--    * right nested +++ gives stack overflow
--    * intersection/difference (generalize blend func in Union)
--    * Clean up interfaces, add Haddock comments
--    * libraries on top
--      - geometry
--      - graphs
