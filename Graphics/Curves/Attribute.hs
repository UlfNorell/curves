{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-| Write-only attributes.
 -}
module Graphics.Curves.Attribute where

-- | Representation of an attribute update for an element of type @a@.
data Assignment a
    -- | Set an attribute
  = forall f b. HasAttribute f a => f b := b
    -- | Modify an attribute
  | forall f b. HasAttribute f a => f b :~ (b -> b)

-- | The type constructor @f@ is such that @f b@ is the type of names of
--   attributes of @a@ of type @b@.
class HasAttribute f a where
  modifyAttribute :: f b -> (b -> b) -> a -> a
  setAttribute    :: f b -> b -> a -> a

  setAttribute t x = modifyAttribute t (const x)

infixl 7 `with`
-- | Apply a sequence of attribute assignments to an object (applied
--   left-to-right).
with :: a -> [Assignment a] -> a
with x as = foldl assign x as
  where
    assign :: a -> Assignment a -> a
    assign a (t := x) = setAttribute t x a
    assign a (t :~ f) = modifyAttribute t f a

