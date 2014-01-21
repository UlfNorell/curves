{-# LANGUAGE DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}
module Graphics.Curves.Trie
  ( Trie
  , empty, lookup, lookupPrefix
  , toList, fromList, union, insert
  , size
  ) where

import Prelude hiding (lookup, foldr)
import Control.Monad
import Control.Applicative hiding (empty)
import Data.Foldable hiding (toList, concatMap)
import Data.Monoid
import Data.Maybe
import Data.List hiding (lookup, foldr, union, insert)
import qualified Data.Map as Map
import Data.Map (Map)

import Test.QuickCheck
import Test.QuickCheck.Function

data Trie a b = Node (Maybe b) (Map a (Trie a b))
  deriving (Functor, Foldable, Eq, Ord)

instance (Show a, Show b) => Show (Trie a b) where
  show t = "[" ++ intercalate ", " (map f (toList t)) ++ "]"
    where
      f (ks, v) = show ks ++ " -> " ++ show v

instance (Ord a, Monoid b) => Monoid (Trie a b) where
  mempty = empty
  mappend (Node v m) (Node v' m') =
    Node (mappend v v') (Map.unionWith mappend m m')

empty :: Trie a b
empty = Node Nothing Map.empty

lookup :: Ord a => [a] -> Trie a b -> Maybe b
lookup []     (Node v _) = v
lookup (k:ks) (Node _ t) = lookup ks =<< Map.lookup k t

-- | Lookup the longest prefix of the key that's in the trie
lookupPrefix :: Ord a => [a] -> Trie a b -> Maybe (b, [a], [a])
lookupPrefix ks (Node v t) =
  case ks of
    []   -> done
    k:ks -> (cons k <$> (lookupPrefix ks =<< Map.lookup k t)) `mplus` done
  where
    done = (,,) <$> v <*> pure [] <*> pure ks
    cons k (v, ks, ks') = (v, k:ks, ks')

toList :: Trie a b -> [([a], b)]
toList (Node v t) =
  [ ([], v) | Just v <- [v] ] ++
  [ (k:ks, v) | (k, t) <- Map.toList t
              , (ks, v) <- toList t ]

singleton :: [a] -> b -> Trie a b
singleton [] v = Node (Just v) Map.empty
singleton (k:ks) v = Node Nothing $ Map.singleton k (singleton ks v)

union :: Ord a => Trie a b -> Trie a b -> Trie a b
union (Node u s) (Node v t) =
  Node (mplus u v) (Map.unionWith union s t)

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert ks v t = union (singleton ks v) t

fromList :: Ord a => [([a], b)] -> Trie a b
fromList = foldr (uncurry insert) empty

size :: Trie a b -> Int
size = foldr (const succ) 0

-- | Gives prededence to the outer trie when there is a conflict.
joinTrie :: Ord a => Trie a (Trie a b) -> Trie a b
joinTrie (Node v t) = union (fromMaybe empty v) (Node Nothing $ joinTrie <$> t)

instance Ord a => Monad (Trie a) where
  return x = singleton [] x
  m >>= k  = joinTrie (fmap k m)

instance Ord a => Applicative (Trie a) where
  pure  = return
  (<*>) = ap

-- Testing ----------------------------------------------------------------

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = Map.fromList <$> arbitrary
  shrink = map Map.fromList . shrink . Map.toList

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Trie a b) where
  arbitrary = fromList <$> arbitrary
  shrink (Node v t) =
    [ empty | isJust v || not (Map.null t) ] ++
    [ t        | t <- Map.elems t ] ++
    [ Node v t | v <- shrink v ] ++
    [ Node v t | t <- shrink t ]

data Bit = O | I
  deriving (Eq, Ord)

instance Show Bit where
  show O = "0"
  show I = "1"
  showList bs = showString (concatMap show bs)

instance Arbitrary Bit where
  arbitrary = elements [O, I]
  shrink O = []
  shrink I = [O]

type T' = Trie Bit
type T = T' Integer

infix 0 ===
x === y = whenFail (putStrLn $ show x ++ " /=\n" ++ show y) (x == y)

prop_monad1 :: T -> Property
prop_monad1 t = (t >>= return) === t

prop_monad2 :: Integer -> Fun Integer T -> Property
prop_monad2 x (Fun _ f) = (return x >>= f) === f x

-- Not true. Reason: m >>= f gives precedence to values with shorter keys in m.
-- Changing bracketing can change when a conflict appears and thus which value
-- gets dropped.
prop_monad3 :: T -> Fun Integer T -> Fun Integer T -> Property
prop_monad3 m (Fun _ f) (Fun _ g) = (m >>= f >>= g) === (m >>= \x -> f x >>= g)

prop_app1 :: Fun Integer Integer -> T -> Property
prop_app1 (Fun _ f) t = pure f <*> t === fmap f t

prop_app2 :: T' (Fun Integer Integer) -> Integer -> Property
prop_app2 f x = fmap apply f <*> pure x === fmap (`apply` x) f

-- Also not true (for the same reason as monad3)
prop_app3 :: T' (Fun (Integer, Integer) Integer) -> T -> T -> Property
prop_app3 f x y = ((apply <$> f) <*> ((,) <$> x <*> y)) ===
                  ((curry . apply <$> f) <*> x <*> y)
