
module ExampleGen where

import Control.Applicative
import Control.Exception
import System.IO
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Graphics.Curves

makeImage name w h i = makeImageT "" name w h i

makeImageT tag name w h i =
  makeImage' tag name w h (autoFit 0 (Vec (fromIntegral w) (fromIntegral h)) i)

makeImage' tag name w h i = do
  hPutStr stderr $ "Rendering " ++ name ++ "... "
  renderImage ("images/" ++ name ++ ".png") w h white i
  hPutStrLn stderr "done"
  putStr $ "<img title='" ++ tag ++ "' src=\"../images/" ++ name ++ ".png\"/>"

header title =
  putStr $ unlines
    [ "<head>"
    , "  <title>" ++ title ++ "</title>"
    , "  <link href='http://fonts.googleapis.com/css?family=Open+Sans|Droid+Sans+Mono&subset=latin,greek-ext' rel='stylesheet' type='text/css'>"
    , "  <link href='examples.css' rel='stylesheet' type='text/css'>"
    , "</head>"
    , "<body>" ]

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
  putStrLn $ "<a name=\"" ++ s ++ "-back\"/><a class=footnote href=\"#" ++ s ++ "\">" ++ show i ++ "</a>"

footnoteDef s = do
  notes <- getFootnotes
  let i = maybe "?" show $ lookup s notes
  putStr $ unlines
    [ "<p><a name=" ++ show s ++ "/>"
    , "<a class=footnote href=\"#" ++ s ++"-back\">▲ " ++ i ++ "</a>" ]

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

done = do
  putStrLn "</body></html>"
  hPutStrLn stderr "Done"

