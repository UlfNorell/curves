
module Graphics.EasyImage.Text.Fonts.Liberation where

import Data.List
import Graphics.EasyImage.SVG.Font
import System.IO.Unsafe
import System.FilePath
import Paths_easy_image

data Modifier = Bold | Italic
  deriving (Show, Eq, Ord)
data Style = Mono | Sans | Serif
  deriving (Show, Eq, Ord)

fontFileName :: Style -> [Modifier] -> IO FilePath
fontFileName s ms = getDataFileName $ "fonts" </> "Liberation" ++ show s ++ "-" ++ m <.> "svg"
  where
    m | null ms   = "Regular"
      | otherwise = concatMap show $ nub $ sort ms

fonts :: [((Style, [Modifier]), SVGFont)]
fonts = [ (key, unsafePerformIO $ loadFont =<< uncurry fontFileName key)
        | s      <- [Mono, Sans, Serif]
        , bold   <- [False, True]
        , italic <- [False, True]
        , let key = (s, [Bold | bold] ++ [Italic | italic])
        ]

liberation :: Style -> [Modifier] -> SVGFont
liberation s ms = font
  where
    Just font = lookup (s, nub $ sort ms) fonts
