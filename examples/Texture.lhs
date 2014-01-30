
%{ header "Texture" }

<!--

> import ExampleGen

-->

> import Graphics.Curves
> import Data.NumInstances

<h1>Textures</h1>

We have <a href=Basics.html>seen</a> how to fill a closed curve with a solid
colour using the <code>FillColour</code> attribute. The <code>Texture</code>
attribute lets you specify a fill colour parameterised by a pixel coordinate
and a texture coordinate. The texture coordinate is given relative to a
customizable texture basis.

<h2>Using the texture coordinate</h2>
First, let's have ourselves an invisible egg

> egg = curve 0 (2 * pi) (\t ->
>           Vec (0.8 * cos t - 0.05 * signum (cos t) * sin t)
>               (0.95 * sin t))
>         `with` [ LineColour := transparent ]

The rest of this page is dedicated to giving the egg pretty colours, but for
reference, this is what it looks like with the <code>LineColour</code> turned
back on.
%{ makeImage "texture-egg" 100 100 (egg `with` [LineColour := black]) }

>
> redTex   _ p = Colour (getX $ abs p) 0 0 1
> greenTex _ p = Colour 0 (0.5 + 0.5 * getY p) 0 1
> redEgg    = egg `with` [ Texture := redTex   ]
> greenEgg  = egg `with` [ Texture := greenTex ]
> yellowEgg = egg `with` [ Texture := redTex + greenTex ]
>
> eggs = redEgg <> translate (Vec 2 0) greenEgg
>             <> translate (Vec 4 0) yellowEgg

%{ makeImage "texture1" 300 100 eggs }

<p>
Texture coordinates are transformed together with the image.

> ex2 = rotate (pi/4) yellowEgg

%{ makeImage "texture2" 100 100 ex2 }

<h3>The TextureBasis</h3>
Texture coordinates can be manipulated directly by changing the
<code>TextureBasis</code> attribute.

> ex3 = yellowEgg `with` [ TextureBasis :~ rotate (pi/4) ]

%{ makeImage "texture3" 100 100 ex3 }

<h2>Using the pixel coordinate</h2>
The pixel coordinate is useful for creating raster effects:

> ex4 = egg `with` [ Texture  := \(Vec x y) _ ->
>                     if even (floor (x / 10) + floor (y / 10))
>                     then Colour 1   0 0 1
>                     else Colour 0.5 0 0 1 ]

%{ makeImage "texture4" 100 100 ex4 }

Transforming the image has no effect on the pixel coordinates:

> ex5 = ex4 <> translate (Vec 1 0) (rotate (-pi/4) ex4)

%{ makeImage "texture5" 150 100 ex5 }

<h2>FillColour and Texture interaction</h2>
The <code>FillColour</code> and <code>Texture</code> attributes map to the same
underlying property. In fact setting the fill colour is (almost)</i>%{ footnote
"almost" } equivalent to setting the texture to a constant function.  This means
that modifying the fill colour of an image that has a texture will update the
texture.

> ex6 = yellowEgg `with`
>         [ FillColour :~ \(Colour r g b a) -> Colour g b r a ]

%{ makeImage "texture6" 100 100 ex6 }

<hr>

<div class=footnotes>
%{ footnoteDef "almost" }
The only difference is that if the fill colour is set to transparent, the fill
blur is not taken into account when computing the bounds of an image. Of
course, if a texture is set we can't decide%{ footnote "well" } if it's the
constant transparent function.

%{ footnoteDef "well" }
Well I guess we could, since there are a finite%{ footnote "lots" } number of
texture function inputs. It would just take a while.

%{ footnoteDef "lots" }
%#{ (2^64)^4 } to be precise.
</div>

%{ done }
