
%{ header "top" "Examples" }

<!--

> import Control.Applicative
> import ExampleGen
> import Graphics.Curves
> import qualified Texture
> import qualified Basics
> import qualified Styles

> makeGrid :: Int -> Int -> [[(String, Image)]] -> IO ()
> makeGrid w h xs = do
>   ts <- mapM sequence [ [ (,) s <$> makeImageT' s w h i | (s, i) <- row ] | row <- xs ]
>   putStr $
>     tag "table class=examples" $ map (tag "tr" . map (tag "td")) (concatMap mkRow ts)
>   where
>     mkRow row = [is, ts]
>       where (is, ts) = unzip $ map mkCell row
>     mkCell (name, img) = (link img, link name)
>       where link = tag ("a href=" ++ name ++ ".html")

-->

<h1>Examples</h1>

%{ makeGrid 50 50 [ [ ("Basics",  Basics.thumbnail)
                    , ("Styles",  Styles.thumbnail)
                    , ("Texture", Texture.thumbnail) ] ] }

<span class="vertical-space"></span>
%{ done }

