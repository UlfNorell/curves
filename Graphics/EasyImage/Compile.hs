
module Graphics.EasyImage.Compile where

import Prelude hiding (minimum, maximum, any, or, and)
import Control.Applicative
import Data.Foldable
import Data.Monoid

import Graphics.EasyImage.Math
import Graphics.EasyImage.BoundingBox
import Graphics.EasyImage.Image
import Graphics.EasyImage.Colour
import Graphics.EasyImage.Curve

import Debug.Trace

-- Compilation ------------------------------------------------------------

type Segments = BBTree (AnnotatedSegment LineStyle)

data FillStyle = FillStyle Colour Scalar LineStyle
data LineStyle = LineStyle Colour Scalar Scalar

instance Monoid LineStyle where
  mempty  = LineStyle transparent 0 0
  mappend (LineStyle c1 w1 b1) (LineStyle c2 w2 b2) =
    LineStyle (c1 `blend` c2) (max w1 w2) (max b1 b2)

data CompiledImage
      = Segments FillStyle Segments
      | CIUnion (Op (Maybe Colour)) BoundingBox CompiledImage CompiledImage
      | CIEmpty

instance HasBoundingBox CompiledImage where
  bounds (Segments fs b) = relaxBoundingBox (max fw lw) $ bounds b
    where
      fw = case fs of
             FillStyle c w _ | not $ isTransparent c -> w
             _ -> 0
      lw = case fs of
             FillStyle _ _ (LineStyle c w b) | not $ isTransparent c -> w + b
             _ -> 0
  bounds (CIUnion _ b _ _) = b
  bounds CIEmpty           = Empty

compileImage :: Image -> CompiledImage
compileImage = compileImage' 1

setLineStyle :: CurveStyle -> AnnotatedSegment (Scalar, Scalar) -> AnnotatedSegment LineStyle
setLineStyle s seg = fmap mkLineStyle seg
  where
    mkLineStyle (d, r) = LineStyle (lineColour s d r) (lineWidth s d r) (lineBlur s d r)

compileImage' :: Scalar -> Image -> CompiledImage
compileImage' res (ICurve c) = Segments fs ss
  where
    s  = curveStyle c
    fs = FillStyle (fillColour s) (fillBlur s) (foldMap annotation ss)
    ss = setLineStyle (curveStyle c) <$> curveToSegments res c
compileImage' res IEmpty = CIEmpty
compileImage' res (Combine blend a b) =
  CIUnion blend (bounds (ca, cb)) ca cb
  where
    ca = compileImage' res a
    cb = compileImage' res b

