{-# LANGUAGE DeriveFunctor, TupleSections #-}
module Graphics.EasyImage.SVG.Font where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Char
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map
import Text.XML.HaXml hiding (with)
import qualified Graphics.EasyImage.Trie as Trie
import Graphics.EasyImage.Trie (Trie)

import Graphics.EasyImage
import Graphics.EasyImage.SVG.Path

type GlyphName = String

data Glyph = Glyph { glyphHorizAdv :: Scalar
                   , glyphName     :: GlyphName
                   , glyphPath     :: Path
                   , glyphChars    :: String
                   }
  deriving Show

data SVGFont = SVGFont { fontId           :: String
                       , fontUnitsPerEm   :: Scalar
                       , fontAscent       :: Scalar
                       , fontDescent      :: Scalar
                       , fontMissingGlyph :: Glyph
                       , fontGlyphs       :: Trie Char Glyph
                       , fontKerning      :: Map (GlyphName, GlyphName) Scalar
                       , fontGlyphsByName :: Map GlyphName Glyph
                       }
  deriving Show

uncomma s = unc (filter (not . isSpace) s)
  where
    unc [] = []
    unc s = w : unc (drop 1 s')
      where (w, s') = break (==',') s

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
        , glyphPath     = maybe [] (parsePath . lexPath) $ attribute "d" c
        , glyphName     = maybe "missing-glyph" (head . uncomma) $ attribute "glyph-name" c
        , glyphChars    = fromMaybe "" $ attribute "unicode" c
        }

svgFont :: Document a -> SVGFont
svgFont (Document _ _ (Elem _ _ c0) _) =
  SVGFont { fontId         = attribute_ "id" font
          , fontUnitsPerEm = attribute' "units-per-em" fontface
          , fontAscent     = attribute' "ascent" fontface
          , fontDescent    = attribute' "descent" fontface
          , fontMissingGlyph = parseGlyph defaultAdv missing
          , fontGlyphs       = glyphMap
          , fontKerning      = Map.fromList $ concatMap mkKern kerning
          , fontGlyphsByName = Map.fromList $ map byName glyphs
          }
  where
    defaultAdv = attribute' "horiz-adv-x" font
    c = xmlUnEscapeContent stdXmlEscaper c0
    [font]     = (tag "defs" /> tag "font") =<< c
    [fontface] = (tag "font" /> tag "font-face") font
    [missing]  = (tag "font" /> tag "missing-glyph") font
    glyphTags  = (tag "font" /> (tag "glyph" `o` attr "unicode")) font
    kerning    = (tag "font" /> tag "hkern") font

    glyphs = map mkGlyph glyphTags
    glyphMap = Trie.fromList glyphs
    byName (_, g) = (glyphName g, g)

    mkKern tag = [ ((x, y), k) | x <- u1 ++ g1, y <-  u2 ++ g2 ]
      where
        k = attribute' "k" tag
        attr t = maybe [] uncomma (attribute t tag)
        u t = [ glyphName g | [c] <- attr t, Just g <- [Trie.lookup [c] glyphMap] ]
        u1 = u "u1"
        u2 = u "u2"
        g1 = attr "g1"
        g2 = attr "g2"
    mkGlyph c = (glyphChars g, g)
      where g = parseGlyph defaultAdv c
    toChar [c] = c
    toChar s = error $ "not a char: \"" ++ s ++ "\""

loadSVGFont :: FilePath -> IO SVGFont
loadSVGFont file = do
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
  fromMaybe (fontMissingGlyph font) $ Trie.lookup [c] (fontGlyphs font)

stringGlyph :: SVGFont -> String -> (Glyph, String, String)
stringGlyph font s =
  fromMaybe (fontMissingGlyph font, take 1 s, drop 1 s) $
  Trie.lookupPrefix s (fontGlyphs font)

drawChar :: SVGFont -> Char -> Image
drawChar font c = drawGlyph $ charGlyph font c

charWidth :: SVGFont -> Char -> Scalar
charWidth font c = glyphHorizAdv $ charGlyph font c

-- | No kerning or combined characters.
drawString_ :: SVGFont -> String -> Image
drawString_ font s = draw 0 Nothing s
  where
    draw p _ [] = mempty
    draw p prev (c:s) = translate p (drawChar font c) <> draw p' (Just c) s
      where
        p' = p + Vec (charWidth font c) 0

drawString :: SVGFont -> String -> Image
drawString font s = draw 0 Nothing s
  where
    draw p _ [] = mempty
    draw p prev (c:s) = translate p' (drawGlyph g) <> draw p'' (Just x) s'
      where
        x = glyphName g
        (g, cs, s') = stringGlyph font (c:s)
        p'  = p - Vec kern 0
        p'' = p' + Vec (glyphHorizAdv g) 0
        kern = fromMaybe 0 $ do
          x' <- prev
          k  <- Map.lookup (x', x) $ fontKerning font
          return k

