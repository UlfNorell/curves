{-# LANGUAGE MultiWayIf #-}
import System.Environment

import Control.Applicative
import Data.List
import Data.Monoid
import System.Environment

import Graphics.EasyImage
import Graphics.EasyImage.Chart
import Graphics.EasyImage.Text
import Graphics.EasyImage.Geometry
import Graphics.EasyImage.Graph
import Graphics.EasyImage.Text.SVG
import Debug.Trace

circle' :: Point -> Scalar -> Image
circle' (Vec x y) r =
  curve (\α -> Vec (x + r * cos α) (y + r * sin α)) 0 (2 * pi) `with` lineStyle 1 1.2 red

ellipse :: Point -> Scalar -> Scalar -> Image
ellipse (Vec x y) r d = curve f 0 (2 * pi) `with` lineStyle 3 5 (opacity 0.7 blue)
  where
    f α = Vec (x + (r - s) * cos α) (y + (r - s) * sin α)
      where
        s = (d^2 + r^2 - 2 * d * r * cos α) / (2 * r - 2 * d * cos α)

save i = renderImage "test.png" 800 600 white i

outline i = (i `with` [ LineWidth := 3, LineColour := white ]) <>
            (i `with` [ LineWidth := 5, LineColour := Colour 0.8 0 0.6 1 ])

main = do
  args <- getArgs
  let s = case args of
            s:_ -> s
            _ -> "AB0d"
  font <- parseFile "fonts/Calligraffiti-webfont.svg"
  save $ autoFit (Vec 20 20) (Vec 780 580) $
    drawString font s `with` [LineColour := transparent, FillColour := black, FillBlur := 0.9]
    -- graph (-1) 1 (\x -> 1 + cos (pi * x)) `with` brushStyle 15 200 <>
    -- graph (-1) 1 (\x -> 1 + x + sin (pi * x) / pi)
    --   `with` ([LineColour := red] ++ brushStyle 10 200)
    -- <> (arrow (-unitX) (unitX) <>
    --  arrow (-0.2 * unitY) (2.2 * unitY))
    --   `with` [LineColour := opacity 0.5 black]
    -- circle 0 3 `with` [ LineWidth 1, LineBlur 20, LineColour red ] <>
    -- poly [-1, Vec 1 (-1), Vec 0 1]
    -- image1
    -- circle (Vec 0 0) 10 `with` [ LineWidth 1, LineBlur 10, LineColour $ opacity 1 red
    --                            , FillColour $ opacity 0.5 blue ] <>
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
    --   `with` [LineWidth := 1, FillColour := opacity 0.5 blue]
    -- <>
    -- rectangle (Vec (-0.2) 2.2) (Vec 26 (-17))
    --   `with` [FillColour := opacity 0.3 red]
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
    -- barChart (map fromIntegral [25,24..1])
    -- line (-unitX) unitX `with` ([LineWidth := 2, LineBlur := 3] ++ dashedOpen 30 50)
    -- circle 0 1 `with` ([LineWidth := 4] ++ gradient red blue 50 ++ dashedClosed 250 10) <>
    -- (mconcat [ line p q | (p, q) <- let ps = [unitX, rotate (4/3 * pi) unitX, rotate (2/3 * pi) unitX] in zip ps (tail ps ++ [head ps]) ])
    --   `with` ([LineWidth := 2, LineBlur := 3] ++ dashedOpen 30 50) <>
    -- (foldr1 (+++) [ fractal 1 p q | (p, q) <- let ps = [unitX, rotate (4/3 * pi) unitX, rotate (2/3 * pi) unitX] in zip ps (tail ps ++ [head ps]) ])
    --   `with` [FillColour := opacity 0.3 blue, LineColour := Colour 0 0 0.5 0.5, FillBlur := 20, LineBlur := 0.8]
    -- drawBBox (circle 0 1)
    -- zipImage (+) (point 0) (differentiate (circle 0 1)) `with` [LineColour := red]
    -- unfreezeImage (outline 1 (bSpline [0, unitY, 1, unitX, 2]))
    -- outline 100 (line 0 unitX)
    -- scale (Vec 1 0.5) $ graph (-pi) (12 * pi) $ \x -> 10 * sin x * cos x ^ 2 + sin (x * 1.7) + ((x - 15)/3)^2
    -- bezierSegment [0, unitY, 1, unitX, Vec 1 (-1), Vec 2 (-1), 2 * unitX]
  where
    outline d i = zipImage (\_ p v -> p + rot90 (d * norm v)) i i' `with` [LineColour := red] <>
                  zipImage (\_ p v -> p - rot90 (d * norm v)) i i' <>
                  i `with` [LineColour := opacity 0.3 blue]
      where i' = differentiate i
    drawBBox i = i <> rectangle p q `with` [LineColour := transparent, FillColour := opacity 0.2 red]
      where Seg p q = imageBounds i
    sampleText = [' '..'~'] ++ delete '\x3a2' ['Α'..'Ω'] ++ delete 'ς' ['α'..'ω']
    combineTest p f =
      translate p $
      {-combine f-} (circle (Vec (-1) 0) 1.75 `with` [FillColour := opacity 0.5 red, VarLineWidth := \_ d -> (1.5 + sin (d * 4 * pi))])
                -- (circle (Vec 1 0) 1.75    `with` [FillColour := opacity 0.9 blue, FillBlur := 0])
    angled xs = zip xs (iterate (\a -> a + 2 * pi / n) 0)
      where n = fromIntegral (length xs)
    text s = mconcat $ zipWith f (iterate (subtract 2.7) 0) (lines s)
      where f y s = translate (Vec 0 y) (stringImage s)
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

image1 =
    rotateAround (Vec 400 325) (pi/6) (ellipse (Vec 400 300) 400 350) <>
    translate (Vec 250 300) (scale 200 $ circle (Vec 0 0) 1) <> point (Vec 250 300) <>
    outline (ellipse (Vec 200 200) 200 120) <>
    (translate (Vec 20 150) $ scale (Vec 30 100) $ graph 0 (8 * pi) f `with`
      [ LineWidth := 2, LineColour := Colour 0 0.5 0.2 1
      , FillColour := opacity 0.3 green ])
  where
    f x = sin x + 0.5 * sin (3 * x)

fractal res p q = fractal' res $
  interpolate p q
  -- \t -> rotateAround c (-t * pi) p
  -- \t -> p + coord t (sin (t * 2 * pi) / 5)
  where
    xAxis = q - p
    yAxis = rot90 xAxis
    coord x y = diag x * xAxis + diag y * yAxis
    c = (p + q) / 2
    r = distance p q / 2

-- | Range 0 to 1.
type Animation a = Scalar -> a

joinAnimations :: [Animation a] -> Animation a
joinAnimations fs = foo $ zip (map fromIntegral [1..]) fs
  where
    n = fromIntegral $ length fs
    foo ((i, f):fs) t
      | t <= i/n  = f (n * t - i + 1)
      | otherwise = foo fs t
    foo [] _ = error "impossible"

splitAnimation :: Int -> Animation a -> [Animation a]
splitAnimation n f =
  [ \t -> f ((t + i) / n') | i <- map fromIntegral [0..n - 1] ]
  where
    n' = fromIntegral n

fractal' res f = curve' (const f) (flip frac) 0 1
  where
    r = res^2
    frac f t
      | squareDistance p q <= r = f t
      | otherwise = flip joinAnimations t $ map frac
                      [ f1, rotateAround p'  (pi/3) f2,
                            rotateAround q' (-pi/3) f2, f3 ]
      where
        [f1, f2, f3] = splitAnimation 3 f
        p  = f 0
        q  = f 1
        p' = f (1/3)
        q' = f (2/3)

-- TODO
--    * Make it compile with ghc-7.0 (also 6.12?)
--    * libraries on top
--      - geometry
--      - graphs
--        - graph property record to configure the graph
--      - charts
--      - text
--        - auto kerning (how?)
--        - formulas (fractions, sub/superscript etc)
--        - read svg font format
--      - fractals, procedural generation stuff
--        - trees?
--    * make use of bindCurve (or similar) to make it easier to do things like
--      the fractal
--    * tidier examples
--    * 3D
--      - shading would require parameterized fill colour
--    * Look at the diagrams package for inspiration
--      - backend for the diagrams package?
--    * Filling disjointed curves
--      - Update documentation
--  BUGS
