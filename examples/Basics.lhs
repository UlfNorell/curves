
%{ header "basics" "Basics" }

<!--

> import ExampleGen

-->

> import Graphics.Curves
> import Graphics.Curves.Geometry

> ex1 = circle (Vec 50 50) 40

%{ makeImage' "" 100 100 ex1 }

> ex2 = ex1 `with` [ FillColour := opacity 0.6 red ]

%{ makeImage' "" 100 100 ex2 }

> ex3 = ex2 <> rectangle (Vec 70 40) (Vec 115 85) `with` [ LineColour := transparent, FillColour := blue ]

%{ makeImage' "" 150 100 ex3 }

> ex4 = ex3 `with` [ LineBlur := 10, FillBlur := 15 ]

%{ makeImage' "" 150 100 ex4 }

> ex5 = autoFit 0 100 $ circle 0 1

%{ makeImage' "" 100 100 ex5 }

> ex6 = curve 0 (2 * pi) $ \t -> Vec t (sin t)

%{ makeImage 314 100 ex6 }

> dropShadow v o i =
>   i <> mapColour (opacity o)
>         (mapImage (\_ p -> p + v) i `with`
>           [LineBlur := 8, FillBlur := 8])
>
> ex7 = ex6 <> dropShadow 3 0.3 ex6

%{ makeImage 314 100 ex7 }

%{ done }
