{-# LANGUAGE DeriveFunctor, TupleSections #-}
{-| This module contains functions to render text using fonts specified in the
    <http://www.w3.org/TR/SVG/fonts.html SVG format>.
-}
module Graphics.Curves.SVG.Font
  ( SVGFont
  , loadFont
  , drawString
  ) where

import Control.Applicative
import Control.Monad
import Data.Map (Map)
import Data.Char
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map
import Text.XML.HaXml hiding (with)
import qualified Graphics.Curves.Trie as Trie
import Graphics.Curves.Trie (Trie)

import Graphics.Curves
import Graphics.Curves.SVG.Path

type GlyphName = String

data Glyph = Glyph { glyphHorizAdv :: Scalar
                   , glyphName     :: GlyphName
                   , glyphPath     :: Path
                   , glyphChars    :: [Char]
                   }

-- | Contains all the data necessary to render text.
data SVGFont = SVGFont { fontId           :: String
                       , fontUnitsPerEm   :: Scalar
                       , fontCapHeight    :: Scalar
                       , fontAscent       :: Scalar
                       , fontDescent      :: Scalar
                       , fontMissingGlyph :: Glyph
                       , fontGlyphs       :: Trie Char Glyph
                       , fontKerning      :: Map (GlyphName, GlyphName) Scalar
                       , fontGlyphsByName :: Map GlyphName Glyph
                       }

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
        , glyphPath     = maybe [] parsePath $ attribute "d" c
        , glyphName     = maybe "missing-glyph" (head . uncomma) $ attribute "glyph-name" c
        , glyphChars    = fromMaybe "" $ attribute "unicode" c
        }

svgFont :: Document a -> SVGFont
svgFont (Document _ _ (Elem _ _ c0) _) =
  SVGFont { fontId         = attribute_ "id" font
          , fontUnitsPerEm = attribute' "units-per-em" fontface
          , fontAscent     = attribute' "ascent" fontface
          , fontCapHeight  = maybe ascent read $ attribute "cap-height" fontface
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
    ascent     = attribute' "ascent" fontface

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

-- | Read a font from an SVG file.
loadFont :: FilePath -> IO SVGFont
loadFont file = do
  s <- readFile file
  return $ svgFont $ xmlParse "debug.out" s

drawGlyph :: Glyph -> Image
drawGlyph g = drawPath (glyphPath g)

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

-- | Render a string in the given font. The text starts at the origin and is
--   scaled to make upper case letters 1 unit high.
drawString :: SVGFont -> String -> Image
drawString font s =
  scale (diag $ 1 / fontCapHeight font) $
  mconcat [ translate (Vec 0 (-l * lineSep)) $ draw 0 Nothing s
          | (l, s) <- zip [0..] $ lines s ]
  where
    lineSep = fontAscent font - fontDescent font
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

