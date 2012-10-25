
module Graphics.EasyImage.Curve where

import Control.Arrow ((***))

import Graphics.EasyImage.Math
import Graphics.EasyImage.Colour
import Graphics.EasyImage.BoundingBox

data Curve = Curve { curveFunction :: Scalar -> Point
                   , curveStart    :: Scalar
                   , curveEnd      :: Scalar
                   , curveStyle    :: CurveStyle
                   }

-- TODO:  * parameterise by distance from start allowing
--          - dashed/dotted lines
--          - colour gradients
--          - varying thickness
data CurveStyle = CurveStyle
      { lineWidth  :: Scalar
      , lineBlur   :: Scalar
      , lineColour :: Colour
      }

defaultCurveStyle =
  CurveStyle { lineWidth  = 0.0
             , lineBlur   = 1.2
             , lineColour = Colour 0 0 0 1 }

curveToSegments :: Scalar -> Curve -> BBTree Segment
curveToSegments r c = buildBBTree $ map (uncurry Seg . (snd *** snd)) $ concatMap subdivide ss
  where
    f  = curveFunction c
    t0 = curveStart c
    t1 = curveEnd c
    res = r^2
    n = 20  -- minimum number of segments
    pairs xs = zip xs (tail xs)

    ss = pairs $ do
      i <- [0..n]
      let t = t0 + (t1 - t0) * fromIntegral i / fromIntegral n
      return (t, f t)

    subdivide s@((t0, p0), (t1, p1))
      | squareDistance p0 p1 > res = concatMap subdivide [((t0, p0), (t, p)), ((t, p), (t1, p1))]
      | otherwise                  = [s]
      where
        t = (t0 + t1) / 2
        p = f t

