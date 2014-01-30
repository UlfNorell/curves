
<!--

> import ExampleGen

-->

> import Graphics.Curves

> data Vec3 = Vec3 Scalar Scalar Scalar deriving Show
> data Seg3 = Seg3 Vec3 Vec3 deriving Show
> dot3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
> abs3 v = sqrt (absSq3 v)
> absSq3 (Vec3 x y z) = x^2 + y^2 + z^2
> cosAngle u v = dot3 u v / (abs3 u * abs3 v)
>
> vmap3 f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
> vzip3 f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
>
> diag3 x = Vec3 x x x
>
> instance Num Vec3 where
>   (+) = vzip3 (+)
>   (*) = vzip3 (*)
>   (-) = vzip3 (-)
>   abs = diag3 . abs3
>   signum = vmap3 signum
>   fromInteger = diag3 . fromInteger
>

> eyePos = Vec3 0 0 2
> eyeRay (Vec x y) = Seg3 eyePos (Vec3 x y 0)

> isectSphere (Seg3 p0 p1) = p0 + diag3 t * v
>   where v@(Vec3 x y z) = p1 - p0
>         Vec3 a b c     = p0
>         t = (-0.5 * sqrt ((2 * dot3 v p0) ^ 2 - 4 * (absSq3 p0 - 1) * absSq3 v) - dot3 v p0) / absSq3 v

> ball lightPos = circle 0 1 `with` [ Texture := \_ p -> lighting p, LineColour := transparent ]
>   where
>     lighting p = Colour x x x 1
>       where ip = isectSphere (eyeRay p)
>             x  = max 0 $ cosAngle (eyePos - ip) (lightPos - ip)
>
> balls = ball (Vec3 1 1 2) <> translate (Vec 2.2 0) (ball $ Vec3 4 2 2)

%eval{ makeImage "lighting1" 300 150 balls }

