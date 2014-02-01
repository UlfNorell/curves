
%{ header "top" "Examples" }

<!--

> import Control.Applicative
> import ExampleGen
> import Graphics.Curves
> import qualified Textures
> import qualified Basics
> import qualified Styles
> import qualified Advanced
> import qualified Blending
> import qualified Fractals
> import qualified Text

> makeGrid :: Int -> Int -> [[(String, String, Image)]] -> IO ()
> makeGrid w h xs = do
>   ts <- mapM sequence [ [ (,,) file name <$> makeImageT' name w h i | (file, name, i) <- row ] | row <- xs ]
>   putStr $
>     tag "table class=examples" $ map (tag "tr") (concatMap mkRow ts)
>   where
>     mkRow row = [is, ts]
>       where (is, ts) = unzip $ map mkCell row
>     mkCell (file, name, img) = (imgtag $ link img, tag "td" $ link name)
>       where link   = tag ("a href=" ++ file ++ ".html")
>             imgtag = tag "td class=example-img"

-->

<h1>Examples</h1>

Here you can find a series of examples on how to use the curves library. Take your
pick from the topics below.

%{ makeGrid 75 75 [ [ ("Basics",   "Basic Curves",    Basics.thumbnail)
                    , ("Styles",   "Styles",          Styles.thumbnail)
                    , ("Blending", "Blending",        Blending.thumbnail)
                    , ("Advanced", "Advanced Curves", Advanced.thumbnail) ],
                    [ ("Text",     "Drawing Text",    Text.thumbnail)
                    , ("Textures", "Textures",        Textures.thumbnail)
                    , ("Fractals", "Fractals",        Fractals.thumbnail) ] ] }

<span class="vertical-space"></span>
%{ done }

