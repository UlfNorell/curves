{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Graphics.EasyImage.Attribute where

data Assignment a where
  (:=) :: HasAttr f a => f b -> b -> Assignment a
  (:~) :: HasAttr f a => f b -> (b -> b) -> Assignment a

class HasAttr f a where
  modifyAttr :: f b -> (b -> b) -> a -> a

setAttr :: HasAttr f a => f b -> b -> a -> a
setAttr t x = modifyAttr t (const x)

assign :: a -> [Assignment a] -> a
assign x as = foldl assn x as
  where
    assn :: a -> Assignment a -> a
    assn a (t := x) = setAttr t x a
    assn a (t :~ f) = modifyAttr t f a

