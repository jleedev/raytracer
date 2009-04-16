{-# LANGUAGE ExistentialQuantification #-}

import List
import System.IO
import Text.Printf
import Control.Monad

comparing :: (Ord a) => (t -> a) -> t -> t -> Ordering
comparing p x y = p x `compare` p y

type Vec3 = (Double,Double,Double)
type Color = Vec3
type Point = Vec3
type Ray = (Point,Vec3)
type Hit = [Double]
type Scene = [AnyShape]
type Camera = (Ray,Vec3,Vec3)
type Image = (Int,Int,[Char])

class (Show s) => Shape s where
    hit :: s -> Ray -> Hit
    normal :: s -> Vec3 -> Vec3

data AnyShape = forall s. Shape s => AnyShape s

instance Shape AnyShape where
    hit (AnyShape s) = hit s
    normal (AnyShape s) = normal s

instance Show AnyShape where
    show (AnyShape s) = show s

data Sphere = Sphere Vec3 Double deriving Show

normalize x = x .* (1/vec_mag x)
vec_mag x = sqrt (vec_mag2 x)
vec_mag2 x = x .*. x
(x1,y1,z1) .*. (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2
(x1,y1,z1) .+ (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
(x1,y1,z1) .- (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)
(x,y,z) .* s = (x*s,y*s,z*s)

instance Shape Sphere where
    hit (Sphere cn r) (u,v) =
        let w = u .- cn
            a = vec_mag2 v
            b = 2*(w .*. v)
            c = vec_mag2 w - r*r
            in quadratic a b c
    normal (Sphere cn r) u = normalize (u .- cn)

--data Plane = Plane Vec3 Vec3

raytrace :: Scene -> Ray -> Color
raytrace scene ray@(u,v) =
    let hits = sortBy (comparing fst) $ do
        s <- scene
        h <- hit s ray
        guard (h > 0)
        let loc = u .+ (v .* h)
            nor = normal s loc
        return (h,(loc,nor,s))
        in case hits of
                [] -> (0,0,0)
                ((_,(loc,nor,shape)):_) -> (1,1,1)

render :: Scene -> Camera -> (Int,Int) -> Image
render scene ((pos,dir),right,up) (width,height) =
    let dat = do y <- [1..height]
                 x <- [1..width]
                 let y' = 2 * fromIntegral y / fromIntegral height - 1
                     x' = 2 * fromIntegral x / fromIntegral height - 1
                     vec = dir .+ (right .* x') .+ (up .* y')
                     ray = (pos,vec)
                     (r,g,b) = raytrace scene ray
                 toByte `fmap` [r,g,b]
    in (width,height,dat)

toByte :: Double -> Char
toByte = toEnum . round . (*255)

testScene :: Scene
testScene = do
    x <- [-5..5]
    y <- [-5..5]
    return . AnyShape $ Sphere (x,y,0) 0.5

testCamera :: Camera
testCamera = ((pos,dir),right,up)
    where pos = (0,0,-10)
          dir = (0,0,1)
          right = (1,0,0)
          up = (0,1,0)

writePNM :: Image -> FilePath -> IO ()
writePNM (width,height,dat) file = withFile file WriteMode $ \h -> do
    hPrintf h "P6\n%d %d\n255\n" width height
    hPutStr h dat

main = do
    let img = render testScene testCamera (400,400)
    writePNM img "test.pnm"

-- ax + b = 0
linear :: Double -> Double -> Double
linear a b = -b/a

-- ax^2 + bx + c = 0
quadratic :: Double -> Double -> Double -> [Double]
quadratic a b c =
    let d = b*b-4*a*c
        in if d > 0
              then [(-b+sqrt d)/(2*a),(-b-sqrt d)/(2*a)]
              else if d == 0
              then [-b/(2*a)] else []