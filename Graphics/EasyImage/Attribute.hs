{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Graphics.EasyImage.Attribute where

data Assignment a where
  (:=) :: HasAttr f a => f b -> b -> Assignment a
  (:~) :: HasAttr f a => f b -> (b -> b) -> Assignment a

class HasAttr f a where
  modifyAttr :: f b -> (b -> b) -> a -> a

setAttr :: HasAttr f a => f b -> b -> a -> a
setAttr t x = modifyAttr t (const x)

with :: a -> [Assignment a] -> a
with x as = foldl assign x as
  where
    assign :: a -> Assignment a -> a
    assign a (t := x) = setAttr t x a
    assign a (t :~ f) = modifyAttr t f a

