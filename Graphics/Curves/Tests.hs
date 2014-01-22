
module Graphics.Curves.Tests where

import Control.Applicative
import Test.QuickCheck
import Graphics.Curves.Math
import Graphics.Curves.BoundingBox

-- Testing ----------------------------------------------------------------

instance Arbitrary Vec where
  arbitrary = Vec <$> arbitrary <*> arbitrary
  shrink (Vec x y) =
    [ Vec x y | x <- shrink x ] ++
    [ Vec x y | y <- shrink y ]

chooseDouble :: (Scalar, Scalar) -> Gen Scalar
chooseDouble (a, b) = do
  let m = 1000000 :: Integer
  n <- choose (0, m)
  return (a + (b - a) * fromIntegral n / fromIntegral m)

x ≈ y = abs (x - y) < 1.0e-3

eqV (Vec x0 y0) (Vec x1 y1) = x0 ≈ x1 && y0 ≈ y1

prop_distanceToSegment (Positive s) (Positive d) =
  forAllShrink (chooseDouble (-0.2, 1.2)) shrink $ \t ->
  forAllShrink (chooseDouble (0, 2 * pi)) shrink $ \α ->
  forAllShrink arbitrary shrink $ \dp ->
    let tr x y = dp + rotate α (Vec x y)
        p0 = tr 0 0
        p1 = tr s 0
        p  = tr (t * s) d
        dist = distance (Seg p0 p1) p
        ans | t <= 0    = distance p0 p
            | t >= 1    = distance p1 p
            | otherwise = d
    in whenFail (putStr $ unlines
                  [ "dist = " ++ show dist
                  , "ans  = " ++ show ans ]
                )
        (dist ≈ ans)

prop_intersectSegment v u (Positive k) (Positive t) =
  getY v /= 0 ==>
  (forAllShrink (chooseDouble (0, 2 * pi))  shrink $ \α ->
   forAllShrink (chooseDouble (-0.1, 1.1)) shrink $ \x ->
   let pt p = u + Vec k k * rotate α p
       p0 = pt $ Vec 0 0
       p1 = pt $ Vec 0 1
       i  = pt $ Vec 0 x
       q0 = pt $ Vec 0 x - v
       q1 = pt $ Vec 0 x + (Vec t t * v)
       l1 = Seg p0 p1
       l2 = Seg q0 q1
       ans = intersectSegment l1 l2
   in
    whenFail (putStr $ unlines [ "l1  = " ++ show l1
                               , "l2  = " ++ show l2
                               , "i   = " ++ show i
                               , "ans = " ++ show ans ]) $
    case x of
      _ | x >= 0.001 && x <= 0.999 -> True  ==> Just True == fmap (eqV i) ans
        | x < -0.001 || x > 1.001  -> True  ==> Nothing == ans
        | otherwise                -> False ==> True
  )

prop_intersectBBox v u (Positive k) (Positive t) =
  getY v /= 0 ==>
  (forAllShrink (chooseDouble (0, 2 * pi))  shrink $ \α ->
   forAllShrink (chooseDouble (0.01, 0.99)) shrink $ \x ->
   let pt p = u + Vec k k * rotate α p
       p0 = pt $ Vec 0 0
       p1 = pt $ Vec 0 1
       i  = pt $ Vec 0 x
       q0 = pt $ Vec 0 x - v
       q1 = pt $ Vec 0 x + (Vec t t * v)
       l1 = Seg p0 p1
       l2 = Seg q0 q1
   in
    whenFail (putStr $ unlines [ "l1  = " ++ show l1
                               , "l2  = " ++ show l2
                               , "i   = " ++ show i ]) $
      intersectBoundingBox l1 (bounds l2)
  )

goodBasis x y =
  not $ or [ eqV x 0
           , eqV y 0
           , dot x (rot90 y) ≈ 0 ]

prop_toBasis o x y p =
  goodBasis x y ==> eqV p (fromBasis b $ toBasis b p)
  where
    b = Basis o x y
