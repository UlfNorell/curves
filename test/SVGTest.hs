
module Main where

import Control.Applicative

import Graphics.Curves
import Graphics.Curves.SVG.Path

main :: IO ()
main = do
  fish <- parsePath <$> readFile "fish.path"
  renderImage "fish.png" 200 200 white $ autoFit 0 (Vec 200 200) $ drawPath fish

