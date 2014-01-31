
%{ header "texture" "Texture" }

<!--

> import ExampleGen
> import Graphics.Curves.Geometry
> import Graphics.Curves.Text.Fonts.Liberation
> import Graphics.Curves.SVG.Font
> import Graphics.Curves.Text
> import Data.Monoid

-->

> import Graphics.Curves
> import Data.NumInstances

<!--

> w = 120 :: Int
> h = 120 :: Int

-->

<h1>Textures</h1>

We have <a href=Basics.html>seen</a> how to fill a closed curve with a solid
colour using the %{vdoc "Style.FillColour"} attribute. The %{vdoc
"Style.Texture"} attribute lets you specify a fill colour parameterised by a
pixel coordinate and a texture coordinate. The texture coordinate is given
relative to a customizable texture basis.

<h2>Using texture coordinates</h2>
First, let's have ourselves an invisible egg

> egg = curve 0 (2 * pi) (\t ->
>           Vec (0.8 * cos t * (1 - 0.12 * sin t))
>               (0.95 * sin t))
>         `with` [ LineColour := transparent ]

The rest of this page is dedicated to giving the egg pretty colours, but for
reference, this is what it looks like with the <code>LineColour</code> turned
back on.
%{ makeImage w h (egg `with` [LineColour := black]) }

Let's have a red texture which is darker closer to the center of the egg and a
green texture that grows darker towards the bottom. Adding%{footnote
"NumInstances"} them together produces a pretty yellow egg.

> redTex   _ p = Colour (distance p 0) 0 0 1
> greenTex _ p = Colour 0 (0.5 + 0.5 * getY p) 0 1
>
> redEgg    = egg `with` [ Texture := redTex   ]
> greenEgg  = egg `with` [ Texture := greenTex ]
> yellowEgg = egg `with` [ Texture := redTex + greenTex ]
>
> eggs = redEgg <> translate (Vec 2 0) greenEgg
>               <> translate (Vec 4 0) yellowEgg

%{ makeImage (3 * w) h eggs }

<p>
Texture coordinates are transformed together with the image, so the texture
sticks with an object when it's transformed as you would expect.

> tilt = rotate (pi/4) yellowEgg

%{ makeImageT "Humpty, no!" w h tilt }

<h3>The texture basis</h3>

Texture coordinates are given in a customizable coordinate system defined by
the %{vdoc "Style.TextureBasis"} attribute of the image. A %{tdoc "Math.Basis"}
consists of three points, (%{vdoc "Math.origin"}, %{vdoc "Math.xUnit"}, and
%{vdoc "yUnit"}) and describes a coordinate system with origin %{code "origin"}
and basis vectors %{code "xUnit - origin"} and %{code "yUnit - origin"} The
texture coordinate of a point %{code "p"} is %{code "Vec s t"}, where
<!--

> prop_texcoord (Basis origin xUnit yUnit) p (Vec s t) =

-->

>   p == diag s * (xUnit - origin) + diag t * (yUnit - origin)

<!--

> pt p = point p `with` [ LineWidth := 4 ]
> font = liberation Serif []
> str p s = label p 14 s
>   -- translate p $ freezeImage 0 $ scale (diag h) $
>   -- drawString font s `with` [ LineColour := transparent, FillColour := black ]

> basisExample = mconcat
>   [ str (xunit + Vec 15 (-12)) "xUnit"
>   , str (yunit + Vec (-27) 7) "yUnit"
>   , str (-Vec 8 6) "origin"
>   , rotateAround tlabel (angle unitX xunit) $ str tlabel "s(xUnit - origin)"
>   , str rlabel "t(yUnit - origin)"
>   , str (p + Vec 12 6) "p"
>   , arrow 0 xunit
>   , arrow 0 yunit
>   , mconcat
>     [ line xunit (xunit * diag s) ++> p
>     , line yunit (yunit * diag t) ++> p
>     ] `with` ([LineColour := Colour 1 0.2 0.2 1] ++ dashedClosed 5 5)
>   , pt p ]
>   where
>     rlabel = ((p + xunit * diag s)/2 + Vec 65 (-5))
>     tlabel = ((p + yunit * diag t)/2 + Vec 0 14)
>     xunit = Vec 100 20
>     yunit = Vec 30 90
>     b = Basis 0 xunit yunit
>     s = 1.4
>     t = 1.2
>     p = fromBasis b (Vec s t)

-->
In picture form:
%{ makeImage 400 220 basisExample }

The %{vdoc "Math.toBasis"} and %{vdoc "Math.fromBasis"} functions lets you
convert between points in the %{vdoc "Math.defaultBasis"} and an arbitrary
basis.

<p>
With the theory out of the way, let's mess with the egg. Modifying the texture
basis in effect applies the corresponding transformation to the texture. For
instance, we can turn the egg texture upside down and move it slightly
off-center as follows:

> upsideDown = yellowEgg `with`
>   [ TextureBasis :~ translate 0.2 . rotate pi ]

%{ makeImage w h upsideDown }

When an image is transformed, the texture basis is also transformed%{footnote
"basis-transform"}. This is true even if we haven't set a texture for the
object, which can lead to slightly surprising behaviour in cases. Compare the
two balls below

> scaled = (ball1 2 <> translate (Vec 4.3 0) ball2 2)
>           `with` [ Texture    := redTex + greenTex
>                  , LineColour := transparent ]
>   where
>     ball1 r = curve 0 (2 * pi) $ \t -> Vec (r * cos t) (r * sin t)
>     ball2 r = scale r (ball1 1)

%{ makeImage (2 * w) h scaled }

The left ball is drawn as a circle with radius two using %{vdoc "curve"},
and the the right ball is circle with radius one scaled by a factor of two. The
difference between the two is that the the left ball has the default texture
basis and the right one a scaled-up basis. Of course, you can always reset the
texture basis of an object if it isn't what you want.

> scaled' = scaled `with` [ TextureBasis := newBasis ]
>   where
>     newBasis = translate (Vec 2.15 0) $ scale 3 defaultBasis

%{ makeImage (2 * w) h scaled' }

<h2>Using pixel coordinates</h2>

Unlike the texture coordinates, pixel coordinates are completely unaffected by
image transformations. The value of the pixel coordinate is always the position
of the pixel being coloured in the final PNG image. One use case for pixel
coordinates is to create raster effects like these:

> chessEgg = egg `with` [ Texture := \(Vec x y) _ ->
>                           if even (floor (x / 10) + floor (y / 10))
>                           then Colour 1   0 0 1
>                           else Colour 0.5 0 0 1 ]

%{ makeImage w h chessEgg }

Transforming the chess egg does nothing to the texture as advertised.

> chessEggs = chessEgg <> translate (Vec 1 0) (rotate (-pi/4) chessEgg)

%{ makeImage (3 * w `div` 2) h chessEggs }

Of course, that we cannot affect the pixel coordinates with transformations
doesn't mean that we can't play around with the texture function itself.

> swirlyEgg = chessEgg `with`
>   [ Texture :~ \tex p q -> tex (rotateAround 60 (2 * distance q 0) p) q ]

%{ makeImage w h swirlyEgg }

Here we used the texture coordinate to change which pixel we sample from the
old texture.

<h2>FillColour and Texture interaction</h2>
The <code>FillColour</code> and <code>Texture</code> attributes map to the same
underlying property. In fact setting the fill colour is (almost%{ footnote
"almost" }) equivalent to setting the texture to a constant function.  This means
that you can change the texture of an image by modifying the fill colour.

> purpleEgg = yellowEgg `with`
>         [ FillColour :~ \(Colour r g b a) -> Colour g b r a ]

%{ makeImage w h purpleEgg }

<hr>

<div class=footnotes>

%{ footnoteDef "NumInstances" }
Thanks to Conal Elliott's nifty <a
href="http://hackage.haskell.org/package/NumInstances">NumInstances</a>
package.

%{ footnoteDef "basis-transform" }
This is what lets the texture stay with an object when it is transformed.

%{ footnoteDef "almost" }
The only difference is that if the fill colour is set to transparent, the fill
blur is not taken into account when computing the bounds of an image. Of
course, if the texture attribute is set we can't decide%{ footnote "well" } if
it's the constant transparent function.

%{ footnoteDef "well" }
Well I guess we could, since there are a finite%{ footnote "lots" } number of
texture function inputs. It would just take a while.

%{ footnoteDef "lots" }
%#{ (2^64)^4 } to be precise.
</div>

%{ done }
