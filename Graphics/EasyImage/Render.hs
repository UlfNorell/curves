
module Graphics.EasyImage.Render where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as B

import qualified Codec.Picture as Codec
import qualified Codec.Picture.Png as Codec

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Image
import Graphics.EasyImage.Compile
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve

import Debug.Trace

-- Rendering --------------------------------------------------------------

sampleSegments :: FillStyle -> Segments -> Point -> Maybe Colour
sampleSegments (FillStyle fillColour fillBlur (LineStyle lineColour lineWidth lineBlur)) s p@(Vec x y) =
  case isLine of
    Nothing -> edge <|> fill
      where
        edge = do
          let b = fillBlur / 2
          d <- distanceAtMost b s p
          guard (d < b)
          let o | isJust fill = 1 - (b - d) / fillBlur
                | otherwise   = 1 - (b + d) / fillBlur
          return $ opacity o fillColour
    Just (α, c) -> Just $ opacity (getAlpha c) $ addFill (getAlpha c) $ setAlpha α c
  where
    isZero x = round (255 * x) == 0
    isLine = do
      (d, seg) <- distanceAtMost' (lineWidth/2 + lineBlur) s p
      let LineStyle c w b = annotation seg
          inner = w/2
          outer = inner + b
          α | d <= inner = 1
            | d >  outer = 0
            | otherwise  = 1 - (d - inner) / b
      guard $ α > 0
      guard $ not $ isZero (getAlpha c)
      return (α, c)

    hasFill = not $ isZero $ getAlpha fillColour
    addFill α' c = maybe c (blend c . opacity (1/α')) fill
    fill | hasFill && odd (length ps) = Just fillColour
         | otherwise                  = Nothing
      where
        BBox x0 _ _ _ = bounds s
        ray = Seg (Vec (x0 - 1) y) p
        ps  | insideBBox p (bounds s) = intersectBBTree (\r l -> maybe [] (:[]) $ intersectSegment r (theSegment l)) ray s
            | otherwise               = []

sampleBBTree :: (a -> Point -> b) -> BBTree a -> Point -> [b]
sampleBBTree sample (Leaf x) p = [sample x p]
sampleBBTree sample (Node b l r) p
  | p `insideBBox` b = sampleBBTree sample l p ++ sampleBBTree sample r p
  | otherwise        = []

sampleImage :: CompiledImage -> Point -> Maybe Colour
sampleImage CIEmpty               p = Nothing
sampleImage (Segments style s)    p = sampleSegments style s p
sampleImage (CIUnion blend b l r) p
  | not $ insideBBox p b = Nothing
  | otherwise            = blend (sampleImage l p) (sampleImage r p)

type Pixel = Codec.PixelRGBA8

toRGBA :: Colour -> Codec.PixelRGBA8
toRGBA (Colour r g b a) = Codec.PixelRGBA8 (f r) (f g) (f b) (f a)
  where
    f x = round (255 * x)

renderCompiledImage :: Int -> Int -> Colour -> CompiledImage -> Codec.Image Pixel
renderCompiledImage w h bg0 i =
  Codec.generateImage (\x y -> sample (Vec (fromIntegral x) (fromIntegral $ h - 1 - y))) w h
  where
    bg = opaque bg0
    sample p = toRGBA $ case sampleImage i p of
      Nothing -> bg0
      Just c  -> blend c bg

saveImage :: FilePath -> Codec.Image Codec.PixelRGBA8 -> IO ()
saveImage file img = B.writeFile file (Codec.encodePng img)

-- | Render an image as a PNG file with a 1-bit alpha channel. Semi-transparent
--   pixels in the image are blended with the given background colour to
--   produce opaque pixels.
renderImage :: FilePath -- ^ File in which to store the image
            -> Int      -- ^ Image width
            -> Int      -- ^ Image height
            -> Colour   -- ^ Background colour
            -> Image    -- ^ Image to render
            -> IO ()
renderImage file w h bg i = saveImage file $ renderCompiledImage w h bg $ compileImage i

