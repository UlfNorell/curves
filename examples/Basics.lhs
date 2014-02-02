
%{ header "basics" "Basics" }

<!--

> module Basics where

> import ExampleGen

-->

<h1>Basics</h1>

The basic entity that makes up all images in the curves library is a curve. A
curve is essentially a (continuous) function from the unit interval to a point
in 2D space, and an image is a collection of curves.

<p>
In this chapter we'll see how to <a href="#simple">create simple images</a>,
how to <a href="#transform">apply transformations</a> to an image, and finally
how to <a href="#render">save an image</a> to disk as a PNG file.

> import Graphics.Curves

<!--

> thumbnail = sineWave
> w = 100 :: Int
> h = 100 :: Int

-->

<a name="simple"></a>
<h2>Creating simple images</h2>

The simplest way to create a curve is using the %{ vdoc "curve" } function

%{ fakeCode "curve :: Scalar -> Scalar -> (Scalar -> Point) -> Image" }

which takes two scalars %{code "a"} and %{code "b"} and a function %{code "f"}
from scalars to points%{footnote "points"} and creates an %{tdoc "Image"} with
a single curve consisting of the points { f t | a ≤ t ≤ b }. For instance,

> sineWave = curve 0 (2 * pi) $ \t -> Vec t (sin t)

%{ makeImage (2 * w) h sineWave }

The section on <a href="#render">rendering</a> below explains how the points of
the curve gets translated to pixels in the final image.

<h3>Combining images</h3>

Images form a monoid, so you can combine them using %{code "mappend"} (or the
friendlier %{vdoc "<>"}):

> cosineWave = curve 0 (2 * pi) $ \t -> Vec t (cos t)
> waves = sineWave <> cosineWave

%{ makeImage (2 * w) h waves }

It makes no difference for this example, but later it will be important to
keep in mind that the left argument goes on top of the right argument. The
chapter on <a href="Blending.html">blending</a> looks at the various ways to
combine images in more detail.

<h3>Extending curves</h3>

Two curves can be concatenated into a single curve using the %{vdoc "+++"}
combinator.

> waves1 = sineWave +++ curve (2 * pi) (3 * pi) (\t -> Vec t (cos t))

%{ makeImage (3 * w) h waves1 }

If the end-points don't coincide, a straight line segment is added to
connect the two%{footnote "disconnected"}.

The combinators %{vdoc "<++"} and %{vdoc "++>"} adds a straight line segment
to either end of a curve.

> waveBlock = botL <++ sineWave ++> botR ++> botL
>   where botL = Vec 0 (-1.2)
>         botR = Vec (2 * pi) (-1.2)

%{ makeImage (2 * w) h waveBlock }

<h3>Shape examples</h3>

With the tools introduced so far we can build some simple geometric figures.
First, a circle%{footnote "circle"}:

> unitCircle = curve 0 (2 * pi) $ \t -> Vec (cos t) (sin t)

%{ makeImage w h unitCircle }

Next, we'll reimplement the %{vdoc "line"} and %{vdoc "poly"} functions from
the library:

> line' p q = curve_ $ \t -> diag (1 - t) * p + diag t * q
> poly' (p:q:ps) = foldl (++>) (line' p q) (ps ++ [p])

The %{vdoc "curve_"} lets you omit the start and end values for the parameter
when they are 0 and 1. Interpolating between two points is sufficiently useful
to warrant its own library function %{vdoc "Math.interpolate"}, so we can
define %{code "line'"} more elegantly as

%{fakeCode "line' p q = curve_ $ interpolate p q"}

The %{vdoc "poly"} function creates a closed%{footnote "lineStrip"} polygon
from the given points so to create a box we just give the four corners.

> box w h = poly [0, Vec w 0, Vec w h, Vec 0 h]
> goldenBox = box ((1 + sqrt 5) / 2) 1

%{ makeImageT "Wow. Such ratio." w h goldenBox }

Regular n-sided polygons are also easy to define%{footnote "regularPoly"} using
%{code "poly"}.

> regularCorners n =
>   [ Vec (cos x) (sin x)
>   | i <- [0..n - 1]
>   , let x = pi/2 + 2 * pi * fromIntegral i / fromIntegral n ]
>
> regularPoly = poly . regularCorners

%{ makeImage w h (regularPoly 5) }

Reordering the vertexes of an odd-sided regular polygon we can make a star:

> interleave []     ys = ys
> interleave (x:xs) ys = x : interleave ys xs
>
> regularStar n = poly $ uncurry interleave
>                      $ splitAt (div n 2 + 1) $ regularCorners n

%{ makeImage w h (regularStar 7) }

<h3>Concatenating vs. combining</h3>
A natural question to ask is: what's the difference between using %{vdoc "+++"}
and using %{vdoc "<>"} on two curves with coinciding concatenation points? The
non-answer is that in the first case you get an image with a single curve and
the second case you get an image with two curves. This difference becomes most
obvious once you start filling curves (see the <a href=Styles.html>chapter on
curve styles</a>) but we can observe it already with the tools we have.

<p>
Let's define a reversed version of the sine wave, which starts at 2π and ends
at 0, but consists of exactly the same points as the previous sine wave.

> sineWaveR = curve 0 (2 * pi) $ \t -> Vec (r t) (sin (r t))
>   where r t = 2 * pi - t

This is in fact a useful operation to have when concatenating curves, so it's
defined in the library as %{vdoc "reverseImage"}. Using this we can define

%{fakeCode "sineWaveR = reverseImage sineWave" }

Now, let's look at the difference between combining and concatenating %{code
"sineWave"} and %{code "sineWaveR"}.

> combined     = sineWave <>  sineWaveR
> concatenated = sineWave +++ sineWaveR

<table class=imgtable><tr>
<td>%{ makeImage (2 * w) h combined }</td>
<td>%{ makeImage (2 * w) h concatenated }</td>
</tr></table>

If you look closely, you can see that the first image, which used %{vdoc "<>"},
is darker and less smooth than the one using %{vdoc "+++"}.  Basically what
happens is that in the combined case the sine wave is drawn twice, once for
each curve, whereas each point on the concatenated curve is only drawn once,
even though the curve passes through it twice. The section on rendering <a
href="#render">below</a> explains the rendering process in more detail.

<a name="transform"></a>
<h2>Transformations</h2>

<a name="render"></a>
<h2>Rendering images</h2>

<div class=footnotes>

%{footnoteDef "points"}
Note that %{tdoc "Math.Point"} is just a synonym for %{tdoc "Math.Vec"}, the
type of 2-dimensional vectors.

%{footnoteDef "disconnected"}
To skip connecting the end-points, use %{vdoc "+.+"} instead.

%{footnoteDef "circle"}
There is a %{vdoc "circle"} combinator in the library which takes the center
and radius of the circle as parameters.

%{footnoteDef "lineStrip"}
The %{vdoc "lineStrip"} combinator is your friend if you don't want to close
the polygon.

%{footnoteDef "regularPoly"}
In fact, %{vdoc "Geometry.regularPoly"} is already defined in the Geometry module.

</div>

%{ done }
