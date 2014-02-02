
module ExampleGen where

import Control.Applicative
import Control.Exception
import Data.IORef
import Data.List
import Data.Char
import System.IO
import System.IO.Unsafe
import System.Directory
import System.Time
import Graphics.Curves
import Graphics.Curves.Text

-- Images -----------------------------------------------------------------

{-# NOINLINE pageNameRef #-}
pageNameRef :: IORef String
pageNameRef = unsafePerformIO $ newIORef (error "Don't forget to set the page name!")

{-# NOINLINE imageCountRef #-}
imageCountRef :: IORef Int
imageCountRef = unsafePerformIO $ newIORef 0

freshImageName = do
  page <- readIORef pageNameRef
  i    <- readIORef imageCountRef
  writeIORef imageCountRef (i + 1)
  return $ page ++ "-" ++ show i

makeImage w h i = makeImageT "" w h i

makeImageT tag w h i =
  putStr =<< makeImageT' tag w h i

makeImageT' tag w h i =
  makeImage'' tag w h (autoFit 0 (Vec (fromIntegral w) (fromIntegral h)) i)

makeImage' tag w h i = putStr =<< makeImage'' tag w h i

makeImage'' tag w h i = do
  name <- freshImageName
  hPutStr stderr $ "Rendering " ++ name ++ "... "
  renderImage ("images/" ++ name ++ ".png") w h white i
  hPutStrLn stderr "done"
  return $ "<img title='" ++ tag ++ "' src=\"../images/" ++ name ++ ".png\"/>"

-- Headers and footers ----------------------------------------------------

header pagename title = do
  writeIORef pageNameRef pagename
  putStr $ unlines
    [ tag "head"
      [ tag "title" title
      , stylesheet "http://fonts.googleapis.com/css?family=Open+Sans|Droid+Sans+Mono&subset=latin,greek-ext"
      , stylesheet "../css/examples.css"
      ]
    , "<body>"
    , tag "span class=header" $
      [ "curves-", version, ": "
      , tag "a href=Examples.html" "Examples", " - "
      , tag "a href=../../dist/doc/html/curves/index.html" "API" ] ++
      [ tag "span class=right"
        [ tag "a href=http://github.com/UlfNorell/curves" "GitHub", " - "
        , tag "a href=http://hackage.haskell.org/package/curves" "Hackage" ] ]
    ]

done = do
  now <- getClockTime
  putStrLn $ unlines
    [ tag "span class=footer"
        [ "curves-", version, ": Ulf Norell &lt;ulf.norell@gmail.com&gt;"
        , tag "span class=right" (show now) ]
    , "</body></html>" ]
  hPutStrLn stderr "Done"

todo = rotate (pi/4) $ translate ( Vec 0 1) (stringImage' CenterAlign 0.4 "Coming") <>
                       translate (-Vec 0 1) (stringImage' CenterAlign 0.4 "soon")

-- Haddock links ----------------------------------------------------------

splitName "" = []
splitName s@(c:_) | not (isUpper c) = [s]
splitName s = case break (=='.') s of
  (w, '.' : s) -> w : splitName s
  (w, "")      -> [w]

vdoc name = haddock "v" ("Graphics.Curves." ++ name)
tdoc name = haddock "t" ("Graphics.Curves." ++ name)

haddock k qname = putStr $
  tag "code" $
  tag ("a href='../../dist/doc/html/curves/" ++ link ++ "'") name
  where
    ws   = splitName qname
    name = last ws
    modu = init ws
    link = intercalate "-" modu ++ ".html#" ++ k ++ ":" ++ escape name

    escape = concatMap esc
    esc c | isAlphaNum c || elem c "._" = [c]
          | otherwise                   = "-" ++ show (fromEnum c) ++ "-"

-- Formatting -------------------------------------------------------------

code = putStr . unwords . map (tag "code") . words

fakeCode code = putStr $ unlines $ map ("> " ++) $ lines code

-- Footnotes --------------------------------------------------------------

type Footnotes = [(String, Int)]

{-# NOINLINE footnoteRef #-}
footnoteRef :: IORef Footnotes
footnoteRef = unsafePerformIO $ newIORef []

getFootnotes :: IO Footnotes
getFootnotes = readIORef footnoteRef

setFootnotes :: Footnotes -> IO ()
setFootnotes = writeIORef footnoteRef

footnote s = do
  notes <- getFootnotes
  let i = 1 + maximum (0 : map snd notes)
  setFootnotes $ (s, i) : notes
  putStr $ "<a name=\"" ++ s ++ "-back\"/><a class=footnote href=\"#" ++ s ++ "\">" ++ show i ++ "</a>"

footnoteDef s = do
  notes <- getFootnotes
  let i = maybe "?" show $ lookup s notes
  putStr $ unlines
    [ "<p><a name=" ++ show s ++ "/>"
    , "<a class=footnote href=\"#" ++ s ++"-back\">▲ " ++ i ++ "</a>" ]

-- Utils ------------------------------------------------------------------

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

tag :: Stringy a => String -> a -> String
tag t x | null s    = "<" ++ t ++ "/>"
        | otherwise = concat ["<", t, ">", toString s, "</", head (words t), ">"]
  where s = toString x

stylesheet url = tag ("link href='" ++ url ++ "' rel='stylesheet' type='text/css'") ""

class Stringy a where
  toString :: a -> String

instance Stringy Char where
  toString c = [c]

instance Stringy a => Stringy [a] where
  toString = concatMap toString

