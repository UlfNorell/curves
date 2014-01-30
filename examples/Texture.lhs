
%{ exampleHeader }

<!--

> import ExampleGen
> import Graphics.Curves
> import Graphics.Curves.Geometry
> import System.IO

-->

<h1>Textures</h1>

We have <a href=Basics.html>seen</a> how to fill a closed curve with a solid
colour using the <code>FillColour</code> attribute. The <code>Texture</code>
attribute lets you specify a fill colour parameterised by a pixel coordinate
and a texture coordinate. The texture coordinate is given relative to a
customizable texture basis.

<h2>Using the texture coordinate</h2>
Here's a simple example:

> egg = curve (\t -> Vec (0.8 * cos t - 0.05 * signum (cos t) * sin t)
>                        (0.95 * sin t)) 0 (2 * pi)
>         `with` [ LineColour := transparent ]
>
> rTex p = Colour (getX $ abs p) 0 0 1
> gTex p = Colour 0 (0.5 + 0.5 * getY p) 0 1
> rEgg = egg `with` [ Texture := \_ p -> rTex p ]
> gEgg = egg `with` [ Texture := \_ p -> gTex p ]
> yEgg = egg `with` [ Texture := \_ p -> rTex p + gTex p ]
>
> eggs = rEgg <> translate (Vec 2 0) gEgg
>             <> translate (Vec 4 0) yEgg

%{ makeImage "texture1" 300 100 eggs }

<p>
Texture coordinates are transformed together with the image.

> ex2 = rotate (pi/4) yEgg

%{ makeImage "texture2" 100 100 ex2 }

<h3>The TextureBasis</h3>
Texture coordinates can be manipulated directly by changing the
<code>TextureBasis</code> attribute.

> ex3 = yEgg `with` [ TextureBasis :~ rotate (pi/4) ]

%{ makeImage "texture3" 100 100 ex3 }

<h2>Using the pixel coordinate</h2>
The pixel coordinate is useful for creating raster effects:

> ex4 = egg `with` [ FillBlur := 10
>                  , Texture  := \(Vec x y) _ ->
>                     if even (floor (x / 10) + floor (y / 10))
>                     then Colour 1   0 0 1
>                     else Colour 0.5 0 0 1 ]

%{ makeImage "texture4" 100 100 ex4 }

Transforming the image has no effect on the pixel coordinates:

> ex5 = ex4 <> translate (Vec 1 0) (rotate (-pi/4) ex4)

%{ makeImage "texture5" 150 100 ex5 }

<h3>FillColour and Texture interaction</h3>
The <code>FillColour</code> and <code>Texture</code> attributes map to the same
underlying property. In fact setting the fill colour is (almost</i>%{ footnote
"almost" }) the same as setting the texture to a constant function.  This means
that modifying the fill colour of an image that has a texture will update the
texture.

> ex6 = yEgg `with` [ FillColour :~ \(Colour r g b a) -> Colour g b r a ]

%{ makeImage "texture6" 100 100 ex6 }

<hr>

<div class=footnotes>
%{ footnoteDef "almost" }
The only difference is that if the fill colour is set to transparent, the fill
blur is not taken into account when computing the bounds of an image.
<p>
</div>

%!{ done }
