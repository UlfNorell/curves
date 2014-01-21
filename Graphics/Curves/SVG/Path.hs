
module Graphics.Curves.SVG.Path
  ( Path, CoordType(..), PathCmd(..)
  , parsePath, drawPath
  ) where

import Data.Char
import Data.Monoid
import Graphics.Curves

-- | A path is a sequence of path commands.
type Path = [PathCmd]

-- | Path commands can use absolute or relative coordinates.
data CoordType = Absolute | Relative
  deriving Show

-- | The path commands specified by <http://www.w3.org/TR/SVG/paths.html#PathData>.
data PathCmd = MoveTo CoordType Point
             | LineTo CoordType Point
             | HorLineTo CoordType Scalar
             | VerLineTo CoordType Scalar
             | BezierTo CoordType [Point]   -- ^ number of points = degree of the Bézier curve
             | SmoothBezierTo CoordType [Point] -- ^ first control point is
                                                -- the mirror of the
                                                -- previous control point
             | ArcTo CoordType Vec Scalar Bool Bool Point
             | ClosePath
  deriving Show

data PathToken = TokNum Scalar
               | TokCmd Char

instance Show PathToken where
  show (TokCmd c)    = [c]
  show (TokNum x) = show x

lexPath :: String -> [PathToken]
lexPath [] = []
lexPath (c:s)
  | isAlpha c   = TokCmd c : lexPath s
  | isNumChar c = case span isNumChar s of
      (d, s') -> TokNum (read (c:d)) : lexPath s'
  | otherwise   = lexPath s
  where
    isNumChar c = isDigit c || elem c "-."

-- | Read a path string.
parsePath :: String -> Path
parsePath s = parse (lexPath s)
  where
    parse ts = case ts of
      [] -> []
      TokCmd 'M' : ts -> args1p 'M' (MoveTo Absolute) ts
      TokCmd 'm' : ts -> args1p 'm' (MoveTo Relative) ts
      TokCmd 'Z' : ts -> ClosePath : parse ts
      TokCmd 'z' : ts -> ClosePath : parse ts
      TokCmd 'L' : ts -> args1p 'L' (LineTo Absolute) ts
      TokCmd 'l' : ts -> args1p 'l' (LineTo Relative) ts
      TokCmd 'H' : ts -> args1 'H' (HorLineTo Absolute) ts
      TokCmd 'h' : ts -> args1 'h' (HorLineTo Relative) ts
      TokCmd 'V' : ts -> args1 'V' (VerLineTo Absolute) ts
      TokCmd 'v' : ts -> args1 'v' (VerLineTo Relative) ts
      TokCmd 'C' : ts -> argsNp 3 'C' (BezierTo Absolute) ts
      TokCmd 'c' : ts -> argsNp 3 'c' (BezierTo Relative) ts
      TokCmd 'S' : ts -> argsNp 2 'S' (SmoothBezierTo Absolute) ts
      TokCmd 's' : ts -> argsNp 2 's' (SmoothBezierTo Relative) ts
      TokCmd 'Q' : ts -> argsNp 2 'Q' (BezierTo Absolute) ts
      TokCmd 'q' : ts -> argsNp 2 'q' (BezierTo Relative) ts
      TokCmd 'T' : ts -> argsNp 1 'T' (SmoothBezierTo Absolute) ts
      TokCmd 't' : ts -> argsNp 1 't' (SmoothBezierTo Relative) ts
      TokCmd 'A' : ts -> argsN 7 'A' (arcTo Absolute) ts
      TokCmd 'a' : ts -> argsN 7 'a' (arcTo Relative) ts
      TokCmd c : _ -> error $ "parsePath: unknown command: " ++ [c]
      TokNum _ : _ -> error $ "parsePath: not a command " ++ show (take 3 ts)
      where
        next c ts = parse (prevCmd c ts)
        prevCmd c ts@(TokNum _ : _) = TokCmd c : ts
        prevCmd c ts                = ts

        arcTo rel [rx, ry, angle, largeArc, sweep, x, y] =
          ArcTo rel (Vec rx ry) angle (largeArc /= 0) (sweep /= 0) (Vec x y)

        args1 :: Char -> (Scalar -> PathCmd) -> [PathToken] -> Path
        args1 c f (TokNum x : ts) = f x : next c ts

        args1p :: Char -> (Vec -> PathCmd) -> [PathToken] -> Path
        args1p c f (TokNum x : TokNum y : ts) = f (Vec x y) : next c ts

        args2p :: Char -> (Vec -> Vec -> PathCmd) -> [PathToken] -> Path
        args2p c f (TokNum x : TokNum y : TokNum x' : TokNum y' : ts) = f (Vec x y) (Vec x' y') : next c ts

        argsN :: Int -> Char -> ([Scalar] -> PathCmd) -> [PathToken] -> Path
        argsN n c f ts
          | all isNum xs = f (map getNum xs) : next c ts'
          where
            (xs, ts') = splitAt n ts

            isNum TokNum{} = True
            isNum _        = False

            getNum (TokNum x) = x

        argsNp :: Int -> Char -> ([Vec] -> PathCmd) -> [PathToken] -> Path
        argsNp n c f = argsN (2 * n) c (f . points)
          where
            points (x:y:xs) = Vec x y : points xs
            points [] = []

data DrawState =
  DrawState { dsCurrentPoint     :: Point
            , dsLastControlPoint :: Point
            , dsStartOfSubCurve  :: Point }

-- | Render a path.
drawPath :: Path -> Image
drawPath p = snd $ foldl draw (DrawState 0 0 0, mempty) p
  where
    draw (ds, i) cmd = case cmd of
      MoveTo ct p -> (newSubCurve p', i +.+ point p')
        where p' = pt ds ct p
      LineTo ct p -> (newPt ds p', i ++> p')
        where p' = pt ds ct p
      HorLineTo ct x -> (newPt ds p', i ++> p')
        where Vec _ y  = dsCurrentPoint ds
              Vec x' _ = pt ds ct (Vec x 0)
              p'       = Vec x' y
      VerLineTo ct y -> (newPt ds p', i ++> p')
        where Vec x _  = dsCurrentPoint ds
              Vec _ y' = pt ds ct (Vec 0 y)
              p'       = Vec x y'
      BezierTo ct ps -> (newPt ds (last ps'), i +++ bezierSegment (dsCurrentPoint ds : ps'))
        where ps' = map (pt ds ct) ps
      SmoothBezierTo ct ps -> (newPt ds (last ps'), i +++ bezierSegment (p0 : cp : ps'))
        where ps' = map (pt ds ct) ps
              p0  = dsCurrentPoint ds
              cp  = 2 * p0 - dsLastControlPoint ds
      ArcTo{} -> error "TODO: elliptical arcs"
      ClosePath -> (newPt ds p, i ++> p)
        where p = dsStartOfSubCurve ds

    pt ds Absolute p = p
    pt ds Relative p = p + dsCurrentPoint ds

    newPt ds p = ds { dsCurrentPoint = p, dsLastControlPoint = p }
    newSubCurve p = DrawState p p p

