
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
sampleSegments (FillStyle fillColour fillBlur (LineStyle lineColour w b)) s p@(Vec x y) =
  case isLine of
    Nothing -> fill <|> do
        let b = fillBlur
        d <- distanceAtMost b s p
        guard (d < b)
        return $ transparency (1 - d/b) fillColour
    Just (α, c) -> Just $ transparency (getAlpha c) $ addFill (getAlpha c) $ setAlpha α c
  where
    isZero x = round (255 * x) == 0
    isLine = do
      (d, seg) <- distanceAtMost' (b + w) s p
      let LineStyle c w b = annotation seg
          α | d <= w    = 1
            | d > w + b = 0
            | otherwise = 1 - (d - w) / b
      guard $ α > 0
      guard $ not $ isZero (getAlpha c)
      return (α, c)

    hasFill = not $ isZero $ getAlpha fillColour
    addFill α' c = maybe c (blend c . transparency (1/α')) fill
    fill | hasFill && odd (length ps) = Just fillColour
         | otherwise                  = Nothing
      where
        BBox x0 _ _ _ = bounds s
        ray = Seg (Vec (x0 - 1) y) p
        ps  | insideBBox p (bounds s) = intersectBBTree (\r l -> maybe [] (:[]) $ intersectSegment r (theSegment l)) ray s
            | otherwise               = []

sampleBBTree :: (a -> Point -> b) -> BBTree a -> Point -> [b]
sampleBBTree sample (Leaf x) p = [sample x p]
sampleBBTree sample (Node b ts) p
  | p `insideBBox` b = concatMap (\t -> sampleBBTree sample t p) ts
  | otherwise        = []

sampleImage :: CompiledImage -> Point -> Maybe Colour
sampleImage CIEmpty            p = Nothing
sampleImage (Segments style s) p = sampleSegments style s p
sampleImage (CIUnion blend ts) p =
  case sampleBBTree sampleImage ts p of
    [] -> Nothing
    cs -> foldr1 blend cs

type Pixel = Codec.PixelRGBA8

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

renderImage :: FilePath -> Int -> Int -> Colour -> Image -> IO ()
renderImage file w h bg i = saveImage file $ renderCompiledImage w h bg $ compileImage i

