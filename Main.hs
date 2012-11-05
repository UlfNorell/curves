{-# LANGUAGE MultiWayIf #-}
import System.Environment

import Control.Applicative
import Data.List
import Data.Monoid

import Graphics.EasyImage
import Graphics.EasyImage.Chart
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

outline i = (i `with` [ LineWidth := 3, LineColour := white ]) <>
            (i `with` [ LineWidth := 5, LineColour := Colour 0.8 0 0.6 1 ])

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
    -- text (unlines $ chunks 22 $ sampleText)
    --   `with` [LineWidth 1, FillColour $ transparency 0.5 blue]
    -- <>
    -- rectangle (Vec (-0.2) 2.2) (Vec 26 (-17))
    --   `with` [FillColour $ transparency 0.3 red]
    -- mconcat [ rotate a $ translate (Vec 100 0) $ freeze 0 $ scale 15 $ stringImage [c]
    --         | (freeze, (c, a)) <- zip (cycle [freezeImage, freezeImageSize, freezeImageOrientation, const id]) $ angled ['A'..'Z'] ] <>
    -- (circle 0 10 <> circle 0 11)`with` [LineColour $ Colour 0.7 0.7 0.7 1]
    -- rotate (10/12 * pi) angleTest
    -- {-angleTest <>-} label (Vec 1 (-0.2)) 20 "α + β + γ = π"
    -- circle 0 10 <> circle (Vec 10 20) 5 <>
    -- translate (Vec 20 0) (rotate (pi/4) $ freezeImageSize 0 $ scale 20 $ stringImage "Test")
    -- angleTest
    -- <> translate (Vec 4 0) (freezeImageSize 0 $ scale 10 $ rotate (pi/4) $ stringImage "Angle test")
    -- <> arrow (Vec 3 0) (Vec 3 1)
    -- <> translate (Vec 3 1.1) (freezeImage 0 $ scale 10 $ stringImage' CenterAlign 0.1 "<Centered text>"))
    -- combineTest (Vec 0 4) unionBlend <>
    -- combineTest (Vec 6 0) intersectBlend <>
    -- combineTest (Vec 0 0) diffBlend <>
    -- combineTest (Vec 6 4) (flip diffBlend)
    -- stringImage "A"
    --line (Vec 0.5 0) (Vec 0.5 2)
    -- <> -- foldl (++>) (line 3 26.5) [Vec 26.5 3, Vec 3 26.5, 3]
    -- line 3 4 +++ line 5 6 +++ line 7 8
    -- let i = curve (\t -> rot t (Vec (1.4 + sin (t * 5)) 0)) 0 (2 * pi)
    -- in i `with` [FillColour $ Colour 1 0.5 0.8 1] <>
    --    rotate 0.02 i `with` [LineColour transparent, FillBlur 10, FillColour black]
    -- mconcat
    --   [ labelledAngle "108°" 40 unitX (rotate (2 * pi / 5) unitX) (rotate (-2 * pi / 5) unitX)
    --   , regularPoly 5
    --   , circle 0 1 `with` [FillColour $ Colour 1 0.6 0.6 1]
    --       <-> regularPoly 5 `with` [FillColour black]
    --   , let i = line (-1) 1 `with` [LineBlur 20, LineColour $ Colour 0.6 0.6 1 1]
    --     in i <> rotate (pi/2) i
    --   ]
    barChart (map fromIntegral [25,24..1])
  where
    sampleText = [' '..'~'] ++ delete '\x3a2' ['Α'..'Ω'] ++ delete 'ς' ['α'..'ω']
    combineTest p f =
      translate p $
      {-combine f-} (circle (Vec (-1) 0) 1.75 `with` [FillColour := transparency 0.5 red, VarLineWidth := \_ d -> (1.5 + sin (d * 4 * pi))])
                -- (circle (Vec 1 0) 1.75    `with` [FillColour := transparency 0.9 blue, FillBlur := 0])
    angled xs = zip xs (iterate (\a -> a + 2 * pi / n) 0)
      where n = fromIntegral (length xs)
    text s = mconcat $ zipWith f (iterate (subtract 2.7) 0) (lines s)
      where f y s = translate (Vec 0 y) (stringImage' LeftAlign 0 s)
    angleTest =
      mconcat [ poly [p0, p1, p2]
              , labelledAngle "α" 40 p0 p1 p2
              , labelledAngle "β" 40 p1 p2 p0
              , labelledAngle "γ" 40 p2 p0 p1
              ]
      where
        p0 = 0
        p1 = Vec 2 0
        p2 = Vec 2 1

chunks n [] = []
chunks n xs = ys : chunks n zs
  where
    (ys, zs) = splitAt n xs

modDouble a b = a - b * fromIntegral (floor (a / b))

gradient c1 c2 a =
  [VarLineColour := \d _ ->
    case modDouble d (2 * a) of
      x | x <= a    -> blend (setAlpha (x / a) c2) c1
        | otherwise -> blend (setAlpha ((2 * a - x) / a) c2) c1
  ]

dashed c a b =
  [VarLineColour := \d _ ->
    case modDouble d (a + b) of
      x | x <= a    -> c
        | otherwise -> transparent
  ]

image1 =
    rotateAround (Vec 400 325) (pi/6) (ellipse (Vec 400 300) 400 350) <>
    translate (Vec 250 300) (scale 200 $ circle (Vec 0 0) 1) <> point (Vec 250 300) <>
    outline (ellipse (Vec 200 200) 200 120) <>
    (translate (Vec 20 150) $ scale (Vec 30 100) $ graph 0 (8 * pi) f `with`
      [ LineWidth := 2, LineColour := Colour 0 0.5 0.2 1
      , FillColour := transparency 0.3 green ])
  where
    f x = sin x + 0.5 * sin (3 * x)

-- TODO
--    * Clean up interfaces, add Haddock comments
--    * wx-style attributes (:=, :~)
--    * libraries on top
--      - geometry
--      - graphs
--    * tidier examples
--    * text
--      - auto kerning (how?)
--      - formulas (fractions, sub/superscript etc)
--    * 3D
--  BUGS
