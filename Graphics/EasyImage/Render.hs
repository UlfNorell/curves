
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

-- Rendering --------------------------------------------------------------

sampleSegments :: CurveStyle -> Segments -> Point -> Maybe Colour
sampleSegments style s p@(Vec x y) =
  case isLine of
    Nothing -> fill <|> do
        let b = fillBlur style
        d <- distanceAtMost b s p
        guard (d < b)
        return $ transparency (1 - d/b) (fillColour style)
    Just (α, c) -> Just $ transparency (getAlpha c) $ addFill $ setAlpha α c
  where
    w = lineWidth style
    b = lineBlur style
    α' = getAlpha (lineColour style 0)
    isZero x = round (255 * x) == 0
    isLine = do
      (d, seg) <- distanceAtMost' (b + w) s p
      let c = lineColour style (distanceFromStart seg)
          α | d <= w    = 1
            | otherwise = 1 - (d - w) / b
      guard $ not $ isZero (getAlpha c)
      return (α, c)

    hasFill = not $ isZero $ getAlpha (fillColour style)
    addFill c = maybe c (defaultBlendFunc c . transparency (1/α')) fill
    fill | hasFill && odd (length ps) = Just $ fillColour style
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
  case cs of
    [] -> Nothing
    cs -> Just $ foldr1 blend cs
  where
    cs = [ c | Just c <- sampleBBTree sampleImage ts p ]

type Pixel = Codec.PixelRGBA8

renderCompiledImage :: Int -> Int -> Colour -> CompiledImage -> Codec.Image Pixel
renderCompiledImage w h bg0 i =
  Codec.generateImage (\x y -> sample (Vec (fromIntegral x) (fromIntegral $ h - 1 - y))) w h
  where
    bg = opaque bg0
    sample p = toRGBA $ case sampleImage i p of
      Nothing -> bg0
      Just c  -> defaultBlendFunc c bg

saveImage :: FilePath -> Codec.Image Codec.PixelRGBA8 -> IO ()
saveImage file img = B.writeFile file (Codec.encodePng img)

renderImage :: FilePath -> Int -> Int -> Colour -> Image -> IO ()
renderImage file w h bg i = saveImage file $ renderCompiledImage w h bg $ compileImage i

