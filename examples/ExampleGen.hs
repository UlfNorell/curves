
module ExampleGen where

import Control.Applicative
import Control.Exception
import System.IO
import System.Directory
import Graphics.Curves

makeImage name w h i = makeImage' name w h (autoFit 0 (Vec (fromIntegral w) (fromIntegral h)) i)

makeImage' name w h i = do
  hPutStr stderr $ "Rendering " ++ name ++ "... "
  renderImage ("images/" ++ name ++ ".png") w h white i
  hPutStrLn stderr "done"
  putStr $ "<img src=\"../images/" ++ name ++ ".png\"/>"

header title =
  putStr $ unlines
    [ "<head>"
    , "  <title>" ++ title ++ "</title>"
    , "  <link href='http://fonts.googleapis.com/css?family=Open+Sans|Droid+Sans+Mono&subset=latin,greek-ext' rel='stylesheet' type='text/css'>"
    , "  <link href='examples.css' rel='stylesheet' type='text/css'>"
    , "</head>"
    , "<body>" ]

getFootnotes :: IO [(String, Int)]
getFootnotes = do
  exist <- doesFileExist ".footnotes"
  if not exist then return []
               else do
      x <- read <$> readFile ".footnotes"
      x `seq` return x

setFootnotes :: [(String, Int)] -> IO ()
setFootnotes notes = writeFile ".footnotes" (show notes)

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
  removeFile ".footnotes" `catchIO` \_ -> return ()
  hPutStrLn stderr "Done"

