{-| The Liberation font from <https://fedorahosted.org/liberation-fonts>.
  -}
module Graphics.Curves.Text.Fonts.Liberation
  ( Modifier(..), FontStyle(..), liberation ) where

import Data.List
import Graphics.Curves.SVG.Font
import System.IO.Unsafe
import System.FilePath
import Paths_curves

data Modifier = Bold | Italic
  deriving (Show, Eq, Ord)

data FontStyle = Mono | Sans | Serif
  deriving (Show, Eq, Ord)

fontFileName :: FontStyle -> [Modifier] -> IO FilePath
fontFileName s ms = getDataFileName $ "fonts" </> "Liberation" ++ show s ++ "-" ++ m <.> "svg"
  where
    m | null ms   = "Regular"
      | otherwise = concatMap show $ nub $ sort ms

fonts :: [((FontStyle, [Modifier]), SVGFont)]
fonts = [ (key, unsafePerformIO $ loadFont =<< uncurry fontFileName key)
        | s      <- [Mono, Sans, Serif]
        , bold   <- [False, True]
        , italic <- [False, True]
        , let key = (s, [Bold | bold] ++ [Italic | italic])
        ]

liberation :: FontStyle -> [Modifier] -> SVGFont
liberation s ms = font
  where
    Just font = lookup (s, nub $ sort ms) fonts
