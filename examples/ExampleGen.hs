
module ExampleGen where

import Control.Applicative
import Control.Exception
import Data.IORef
import Data.List
import Data.Char
import System.IO
import System.IO.Unsafe
import System.Directory
import Graphics.Curves

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
  makeImage' tag w h (autoFit 0 (Vec (fromIntegral w) (fromIntegral h)) i)

makeImage' tag w h i = do
  name <- freshImageName
  hPutStr stderr $ "Rendering " ++ name ++ "... "
  renderImage ("images/" ++ name ++ ".png") w h white i
  hPutStrLn stderr "done"
  putStr $ "<img title='" ++ tag ++ "' src=\"../images/" ++ name ++ ".png\"/>"

-- Headers and footers ----------------------------------------------------

header pagename title = do
  writeIORef pageNameRef pagename
  putStr $ unlines
    [ "<head>"
    , "  <title>" ++ title ++ "</title>"
    , "  <link href='http://fonts.googleapis.com/css?family=Open+Sans|Droid+Sans+Mono&subset=latin,greek-ext' rel='stylesheet' type='text/css'>"
    , "  <link href='examples.css' rel='stylesheet' type='text/css'>"
    , "</head>"
    , "<body>" ]

done = do
  putStrLn "</body></html>"
  hPutStrLn stderr "Done"

-- Haddock links ----------------------------------------------------------

splitName "" = []
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
    esc c | isAlphaNum c = [c]
          | otherwise    = "-" ++ show (fromEnum c) ++ "-"

-- Formatting -------------------------------------------------------------

code = putStr . unwords . map (tag "code") . words

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

tag t s = concat ["<", t, ">", s, "</", head (words t), ">"]

