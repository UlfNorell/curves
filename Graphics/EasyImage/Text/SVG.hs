
module Graphics.EasyImage.Text.SVG where

import Control.Applicative
import Data.Map (Map)
import Data.Char
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map
import Text.XML.HaXml hiding (with)

import Graphics.EasyImage

type Path = [PathCmd]

data CoordType = Absolute | Relative
  deriving Show

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

parsePath :: [PathToken] -> Path
parsePath ts = case ts of
  [] -> []
  TokCmd 'M' : ts -> args1p 'M' (MoveTo Absolute) ts
  TokCmd 'm' : ts -> args1p 'm' (MoveTo Relative) ts
  TokCmd 'Z' : ts -> ClosePath : parsePath ts
  TokCmd 'z' : ts -> ClosePath : parsePath ts
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
    parse c ts = parsePath (prevCmd c ts)
    prevCmd c ts@(TokNum _ : _) = TokCmd c : ts
    prevCmd c ts                = ts

    arcTo rel [rx, ry, angle, largeArc, sweep, x, y] =
      ArcTo rel (Vec rx ry) angle (largeArc /= 0) (sweep /= 0) (Vec x y)

    args1 :: Char -> (Scalar -> PathCmd) -> [PathToken] -> Path
    args1 c f (TokNum x : ts) = f x : parse c ts

    args1p :: Char -> (Vec -> PathCmd) -> [PathToken] -> Path
    args1p c f (TokNum x : TokNum y : ts) = f (Vec x y) : parse c ts

    args2p :: Char -> (Vec -> Vec -> PathCmd) -> [PathToken] -> Path
    args2p c f (TokNum x : TokNum y : TokNum x' : TokNum y' : ts) = f (Vec x y) (Vec x' y') : parse c ts

    argsN :: Int -> Char -> ([Scalar] -> PathCmd) -> [PathToken] -> Path
    argsN n c f ts
      | all isNum xs = f (map getNum xs) : parse c ts'
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

data Glyph = Glyph { glyphHorizAdv :: Scalar
                   , glyphPath     :: Path
                   }
  deriving Show

data SVGFont = SVGFont { fontId           :: String
                       , fontUnitsPerEm   :: Scalar
                       , fontAscent       :: Scalar
                       , fontDescent      :: Scalar
                       , fontMissingGlyph :: Glyph
                       , fontGlyphs       :: Map Char Glyph
                       }
  deriving Show

attribute :: String -> Content i -> Maybe String
attribute attr (CElem (Elem _ as _) _) = do
  AttValue [(Left s)] <- lookup (N attr) as
  return s
attribute _ _ = Nothing

attribute_ attr c =
  case attribute attr c of
    Just a -> a
    Nothing -> error $ "No attribute: " ++ attr

attribute' :: Read a => String -> Content i -> a
attribute' attr c =
  case reads (attribute_ attr c) of
    [(x, "")] -> x
    _         -> error $ "Bad attribute: " ++ show (attribute_ attr c)

parseGlyph :: Scalar -> Content i -> Glyph
parseGlyph defaultAdv c =
  Glyph { glyphHorizAdv = maybe defaultAdv read $ attribute "horiz-adv-x" c
        , glyphPath     = maybe [] (parsePath . lexPath) $ attribute "d" c }

svgFont :: Document a -> SVGFont
svgFont (Document _ _ (Elem _ _ c0) _) =
  SVGFont { fontId         = attribute_ "id" font
          , fontUnitsPerEm = attribute' "units-per-em" fontface
          , fontAscent     = attribute' "ascent" fontface
          , fontDescent    = attribute' "descent" fontface
          , fontMissingGlyph = parseGlyph defaultAdv missing
          , fontGlyphs       = Map.fromList $ map mkGlyph glyphs
          }
  where
    defaultAdv = attribute' "horiz-adv-x" font
    c = xmlUnEscapeContent stdXmlEscaper c0
    [font]     = (tag "defs" /> tag "font") =<< c
    [fontface] = (tag "font" /> tag "font-face") font
    [missing]  = (tag "font" /> tag "missing-glyph") font
    glyphs     = (tag "font" /> tag "glyph") font

    mkGlyph c = (toChar $ attribute_ "unicode" c, parseGlyph defaultAdv c)
    toChar [c] = c
    toChar s = error $ "not a char: \"" ++ s ++ "\""

parseFile :: FilePath -> IO SVGFont
parseFile file = do
  s <- readFile file
  return $ svgFont $ xmlParse "debug.out" s

data DrawState =
  DrawState { dsCurrentPoint     :: Point
            , dsLastControlPoint :: Point
            , dsStartOfSubCurve  :: Point }

drawGlyph :: Glyph -> Image
drawGlyph g = snd $ foldl drawPath (DrawState 0 0 0, mempty) (glyphPath g)
  where
    drawPath (ds, i) cmd = case cmd of
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

charGlyph :: SVGFont -> Char -> Glyph
charGlyph font c =
  fromMaybe (fontMissingGlyph font) $ Map.lookup c (fontGlyphs font)

drawChar :: SVGFont -> Char -> Image
drawChar font c = drawGlyph $ charGlyph font c

charWidth :: SVGFont -> Char -> Scalar
charWidth font c = glyphHorizAdv $ charGlyph font c

drawString :: SVGFont -> String -> Image
drawString font s = draw 0 s
  where
    draw p [] = mempty
    draw p (c:s) = translate p (drawChar font c) <> draw p' s
      where
        p' = p + Vec (charWidth font c) 0
