{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Fractals where

import Prelude hiding (sum, minimum, foldr1, concatMap, concat, all)
import Control.Arrow ((***), (&&&))
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Random
import Data.List hiding (sum, minimum, foldr1, concatMap, concat, all)
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Foldable
import Graphics.EasyImage
import Graphics.EasyImage.Geometry

-- Trees ------------------------------------------------------------------

box :: Point -> Image
box p = freezeImageSize p $ rectangle (p - 3) (p + 3)

type Volume = (Point -> Bool, Segment)

data TreeConfig = TreeCfg
  { treeVolume                :: Volume
  , treeDensity               :: Int
  , treeStepDistance          :: Scalar
  , treeRadiusOfInfluence     :: Scalar
  , treeKillDistance          :: Scalar
  , treeBranchThicknessFactor :: Scalar
  , treeLeafThickness         :: Scalar
  }

defaultTree :: TreeConfig
defaultTree =
  TreeCfg { treeVolume = ( \p -> let d = distance (p / Vec 2 1) unitY
                                 in d > 0.0 && d < 0.9 && getY p > 0.3 -- (1.1 - d)
                         , Seg (Vec (-2) 0) (Vec 2 2) )
          , treeDensity               = 200
          , treeRadiusOfInfluence     = 0.4
          , treeKillDistance          = 0.2
          , treeStepDistance          = 0.01
          , treeBranchThicknessFactor = 1.7
          , treeLeafThickness         = 0.02
          }

seedVolume :: (Applicative m, MonadRandom m) => TreeConfig -> m [Point]
seedVolume cfg = seed (treeDensity cfg)
  where
    (inside, Seg p0 p1) = treeVolume cfg
    seed 0 = return []
    seed n = do
      p <- Vec <$> getRandomR (getX p0, getX p1) <*> getRandomR (getY p0, getY p1)
      if inside p
        then (p :) <$> seed (n - 1)
        else seed n

data Tree a = Node a [Tree a]
  deriving (Functor, Foldable, Eq)

value (Node x _) = x

leaf x = Node x []

leaves (Node x []) = [x]
leaves (Node _ t)  = concatMap leaves t

bottomUp :: (a -> [b] -> b) -> Tree a -> Tree b
bottomUp f (Node x ts) = Node (f x (map value ts')) ts'
  where
    ts' = map (bottomUp f) ts

topDown :: (a -> [a] -> b) -> Tree a -> Tree b
topDown f (Node x ts) = Node (f x (map value ts)) (map (topDown f) ts)

scanDown :: (a -> b -> (a, c)) -> a -> Tree b -> Tree c
scanDown f z (Node x ts) = Node y (map (scanDown f z') ts)
  where (z', y) = f z x

mapBranchesUp :: (a -> [Tree a] -> [Tree a]) -> Tree a -> Tree a
mapBranchesUp f (Node x ts) = Node x $ f x $ map (mapBranchesUp f) ts

mapBranchesDown :: (a -> [Tree a] -> [Tree a]) -> Tree a -> Tree a
mapBranchesDown f (Node x ts) = Node x $ map (mapBranchesDown f) (f x ts)

extendTree :: Tree a -> (a -> Maybe (Tree a)) -> Tree a
extendTree (Node x ts) f =
  Node x $ [ t | Just t <- [f x] ] ++ map (`extendTree` f) ts

branchThickness :: TreeConfig -> Tree Point -> Tree (Point, Scalar)
branchThickness cfg = bottomUp thick
  where
    k = treeBranchThicknessFactor cfg
    thick x [] = (x, treeLeafThickness cfg)
    thick x ts = (x, parentThickness cfg $ map snd ts)

parentThickness :: TreeConfig -> [Scalar] -> Scalar
parentThickness cfg ws = sum (map (** k) ws) ** (1/k)
  where k = treeBranchThicknessFactor cfg

wireFrameTree :: Tree Point -> Image
wireFrameTree (Node p []) = point p
wireFrameTree (Node p ts) = foldr1 (+.+) $ map ((p <++) . wireFrameTree) ts

renderThickness :: Tree (Point, Scalar) -> Image
renderThickness t = foldMap render
                  $ scanDown addDir (fst (value t) - v) t
  where
    v = unitY
    addDir p0 (p, d) = (p, (p - p0, p, d))

    render :: (Vec, Point, Scalar) -> Image
    render (v, p, d) = line (p - u) (p + u)
      where
        u = diag (d / 2) * norm (rot90 v)

repeatWhileProgress :: (a -> Writer Any a) -> a -> a
repeatWhileProgress f x
  | getAny progress = repeatWhileProgress f y
  | otherwise       = y
  where
    (y, progress) = runWriter (f x)

pruneTree cfg t = mapBranchesDown prune t
  where
    -- prune _ [t] = [t]
    prune (p, d) ts = repeatWhileProgress f ts
      -- concatMap pr $ pairwise merge $ concatMap pr ts
      where
        progress = tell (Any True)
        f ts = pairwise merge =<< (concat <$> mapM pr ts)
        pr t@(Node (q, _) ts)
          | distance p q < d/2 = do progress; concat <$> mapM pr ts
          | otherwise          = return [t]

        pairwise f (x:y:xs) =
          case f x y of
            Nothing -> (x :) <$> pairwise f (y:xs)
            Just z  -> progress >> pairwise f (z:xs)
        pairwise f xs = return xs

        merge (Node (p1, w1) ts1) (Node (p2, w2) ts2)
          | d < (w1 + w2) / 2 = Just (Node (p, w) (ts1 ++ ts2))
          | otherwise         = Nothing
          where w = parentThickness cfg [w1, w2]
                p = (p1 + p2) / 2
                d = min (distance (Seg p p1) p2) (distance (Seg p p2) p1)

renderTree :: TreeConfig -> Vec -> Tree (Point, Scalar) -> Image
renderTree cfg v t =
  poly . untangle
       . outline
       . topDown branchPts
       . fmap fleshOut
       . mapBranchesUp sortBranches
       . scanDown addDir (fst (value t) - v)
       . pruneTree cfg
       $ t
  where
    addDir p0 (p, d) = (p, (p - p0, p, d))
    sortBranches (v, p, d) ts = sortBy (cmp `on` value) ts
      where
        cmp (_, q1, _) (_, q2, _)
          | q1 == q2  = EQ
          | otherwise = compare (angle (-v) (q2 - p)) (angle (-v) (q1 - p))
          where
    fleshOut (v, p, d) = (Seg (pl - v) pl, Seg (pr - v) pr)
      where
        pl = p + diag (d/2) * i
        pr = p - diag (d/2) * i
        i  = norm $ rot90 v
    branchPts (Seg _ l0, Seg _ r0) [] = [(l0 + r0) / 2]
    branchPts (l, r) lrs = map (unlist isect)
                         $ chunks 2
                         $ [Left l] ++ concatMap list lrs ++ [Right r]
      where
        list (x, y) = [Left x, Right y]
        unlist f [x, y] = f x y
        unlist f _ = error "unlist"
        isect l1 l2 = fromMaybe def $ isect' (untag l1) (untag l2)
          where
            untag = either id id
            def = (def1 + def2) / 2
            def1 = case l1 of
                     Left  (Seg _ p) -> p
                     Right (Seg p _) -> p
            def2 = case l2 of
                     Left (Seg p _)  -> p
                     Right (Seg _ p) -> p
        isect' l1 l2
          | a < pi/4  = intersectLineSegment l1 l2 `mplus` 
                        intersectLineSegment l2 l1
          | otherwise = intersectLine l1 l2
          where
            vec (Seg p q) = q - p
            swp (Seg p q) = Seg q p
            ang = angle `on` vec
            a = minimum [ f (h l1) l2 | f <- [ang, flip ang]
                                      , h <- [id, swp] ]

    outline (Node ps ts) = merge (map (:[]) ps) (map outline ts)
      where
        merge (x:xs) (y:ys) = x ++ y ++ merge xs ys
        merge [x]    []     = x
        merge _      _      = error "impossible: merge"

    untangle (p0 : p1 : p2 : p3 : p4 : ps) =
      case intersectSegment (Seg p0 p1) (Seg p2 p3) of
        Nothing ->
          case intersectSegment (Seg p0 p1) (Seg p3 p4) of
            Nothing -> p0 : untangle (p1 : p2 : p3 : p4 : ps)
            Just p  -> untangle (p0 : p : p4 : ps)
        Just p  -> untangle (p0 : p : p3 : p4 : ps)
    untangle ps = ps

a <||> b = a <> translate (Vec (getX qa - getX pb) 0) b
  where
    Seg pa qa = imageBounds a
    Seg pb qb = imageBounds b

b <^^> a = a <> translate (Vec 0 (getY qa - getY pb)) b
  where
    Seg pa qa = imageBounds a
    Seg pb qb = imageBounds b

chunks n [] = []
chunks n xs = ys : chunks n zs
  where (ys, zs) = splitAt n xs

grid :: [[Image]] -> Image
grid = foldr1 (<^^>) . map (foldr1 (<||>))

growTree :: TreeConfig -> [Point] -> Point -> Int -> ([Point], Tree Point)
growTree cfg aps root n = grow aps (Node root []) n
  where
    grow []  t _ = ([], t)
    grow aps t 0 = (aps, t)
    grow aps t n = grow aps' t' (n - 1)
      where
        nodes     = toList t
        closest p =
          case [ (d, n) | n <- nodes, let d = distance p n, d < treeRadiusOfInfluence cfg ] of
            []  -> Nothing
            dns -> Just $ minimum dns
        annotated = map (id &&& closest) aps
        aps' = [ a | a <- aps, treeKillDistance cfg < minimum (map (distance a) (toList t')) ]

        t' | t1 /= t = t1
           | otherwise =
            case leaves t1 of
              [lf] -> extendTree t1 $ \p ->
                if p == lf then Just $ leaf $ p + diag (treeStepDistance cfg) * unitY
                           else Nothing
              _ -> t1

        t1 = extendTree t $ \p ->
          let aps' = [ (a, d) | (a, Just (d, n)) <- annotated, n == p ]
              v    = norm $ sum [ norm (a - p) | (a, _) <- aps' ]
              p'   = p + diag (treeStepDistance cfg) * v
              noProg = all (\(a, d) -> distance a p' > d) aps'
          in if null aps' || v == 0 || noProg
             then Nothing
             else Just $ leaf p'

randomTree :: (Applicative m, MonadRandom m) => TreeConfig -> m Image
randomTree cfg = do
  ps <- seedVolume cfg
  let tree n =
        let tt = pruneTree cfg (branchThickness cfg t) in
        -- foldMap (point . fst) tt `with` [LineColour := red] <>
        -- wireFrameTree (fst <$> tt) `with` [LineColour := opacity 0.3 blue] <>
        -- renderThickness tt `with` [LineColour := opacity 0.3 red] <>
        renderTree cfg unitY (branchThickness cfg t) -- `with` [LineColour :~ opacity 0.2]
        -- <> mconcat (map box ps') `with` [LineColour := opacity 0.3 red]
        where (ps', t) = growTree cfg ps 0 n
  return $ tree 1000
    -- <> mconcat (map box ps) `with` [LineColour := opacity 0.3 blue]
    -- grid $ chunks 3 $ map tree $ take 6 [100,140..]

-- Snow flake fractal -----------------------------------------------------

fractal res p q = fractal' res $
  interpolate p q
  -- \t -> rotateAround c (-t * pi) p
  -- \t -> p + coord t (sin (t * 2 * pi) / 5)
  where
    xAxis = q - p
    yAxis = rot90 xAxis
    coord x y = diag x * xAxis + diag y * yAxis
    c = (p + q) / 2
    r = distance p q / 2

-- | Range 0 to 1.
type Animation a = Scalar -> a

joinAnimations :: [Animation a] -> Animation a
joinAnimations fs = foo $ zip (map fromIntegral [1..]) fs
  where
    n = fromIntegral $ length fs
    foo ((i, f):fs) t
      | t <= i/n  = f (n * t - i + 1)
      | otherwise = foo fs t
    foo [] _ = error "impossible"

splitAnimation :: Int -> Animation a -> [Animation a]
splitAnimation n f =
  [ \t -> f ((t + i) / n') | i <- map fromIntegral [0..n - 1] ]
  where
    n' = fromIntegral n

fractal' res f = curve' (const f) (flip frac) 0 1
  where
    r = res^2
    frac f t
      | squareDistance p q <= r = f t
      | otherwise = flip joinAnimations t $ map frac
                      [ f1, rotateAround p'  (pi/3) f2,
                            rotateAround q' (-pi/3) f2, f3 ]
      where
        [f1, f2, f3] = splitAnimation 3 f
        p  = f 0
        q  = f 1
        p' = f (1/3)
        q' = f (2/3)

