
module Graphics.Curves.Graph (graph) where

import Data.Monoid
import GHC.Float

import Graphics.Curves
import Graphics.Curves.Geometry
import Graphics.Curves.Text

-- | ceiling' k x = smallest n * k with integer n and n * k ≥ x
ceiling' :: Scalar -> Scalar -> Scalar
ceiling' k x = k * fromIntegral (ceiling (x / k))

grades :: Int -> Scalar -> Scalar -> (Int, [Scalar])
grades n a b = (prec, takeWhile (<= b) $ iterate (+step) (ceiling' step a))
  where
    d = b - a
    k = d / fromIntegral n
    log10 x = log x / log 10
    base = log10 k
    ibase = fromIntegral (floor base)
    fbase = base - ibase
    base2 = log10 2
    base5 = log10 5
    coef | fbase < base2 / 2           = 1
         | fbase < (base2 + base5) / 2 = 2
         | fbase < (base5 + 1) / 2     = 5
         | otherwise                   = 10
    step = coef * 10 ** ibase
    prec = round $ max 0 (-ibase)

data TextPos = Below | Above | LeftOf | RightOf

axis :: TextPos -> Scalar -> Scalar -> Scalar -> Scalar -> Image
axis tp a b bot top =
  (arrow p q <> gradeMarks) `with` [LineColour :~ opacity 0.7]
  where
    d = b - a
    p = diag (a - 0.1 * d) * unitX
    q = diag (b + 0.1 * d) * unitX
    (prec, gs) = grades 10 a b
    gradeMarks = mconcat $ map mark gs
    mark x | abs x < d/1000 = mempty
    mark x = freezeImageSize c
        (line (c - 3 * unitY) (c + 3 * unitY) <> text x) <>
        line (c + Vec 0 bot) (c + Vec 0 top) `with` [LineColour := Colour 0.7 0.7 1 1, LineBlur := 0.8]
      where
        c = diag x * unitX
        text x = case tp of
          Below  -> translate (c - 20 * unitY) $ scale 6 $ stringImage' CenterAlign 0.3 s
          LeftOf -> translate (c + Vec (-6) 10) $ rotate (-pi/2) $ scale 6 $ stringImage' RightAlign 0.3 s
          where
            s = formatRealFloat FFFixed (Just prec) x

-- | Draw the graph of a function together with axis and some guides.
graph :: Scalar -> Scalar -> (Scalar -> Scalar) -> Image
graph x0 x1 f = g <> axis Below (getX p) (getX q) (getY p) (getY q)
                  <> rotate (pi/2) (axis LeftOf (getY p) (getY q) (-getX p) (-getX q))
                  <> fx0 <++ g ++> fx1 ++> fx0 `with` [LineColour := transparent, FillColour := Colour 1 0.6 0.6 0.4]
  where
    fx0 = Vec x0 0
    fx1 = Vec x1 0
    g = curve (\x -> Vec x (f x)) x0 x1
    Seg p q = imageBounds g
    w = getX (q - p)
    h = getY (q - p)

