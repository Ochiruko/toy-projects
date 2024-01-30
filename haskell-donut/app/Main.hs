module Main where

import qualified Graphics.Gloss as G
import Donut
import Data.List (sortBy)

mainWindow :: G.Display
mainWindow = G.InWindow "Spinning Donut" (600, 600) (0, 0)

background :: G.Color
background = G.white

θ1Partitions :: Float
θ2Partitions :: Float
θ1Partitions = 60
θ2Partitions = 30

donutRadius1 :: Float
donutRadius2 :: Float
donutRadius1 = 150
donutRadius2 = 75 

donutColor = G.red

rvec :: Vec3 Float
rvec = Vec3 (1, 0, 0)

period :: Float
period = 6

shade :: Float -> G.Color -> G.Color
shade l c
 = let  (r, g, b, a)    = G.rgbaOfColor c
   in   G.makeColor (r * l) (g * l) (b * l) a

-- Use monads to carry height and alpha in future?
-- (surface_polygon, alpha, z)
pictureAtTime :: Float -> G.Picture
pictureAtTime t
  = G.Pictures 
    . map    ( \(x,y,z) -> G.Color (fill y) $ G.Polygon x) 
    . sortBy ( \(_,_,z) (_,_,z') -> compare z z' )
    $ [ (poly θ1 θ2, nz θ1 θ2, z θ1 θ2) | θ1 <- θ1s, θ2 <- θ2s ]
  where
    poly θ1 θ2 = map ( \(Vec3 (x, y, z)) -> (x, y) )
      [ donut donutRadius1 donutRadius2 rvec rθ θ1 θ2
      , donut donutRadius1 donutRadius2 rvec rθ (θ1 + dθ1) θ2
      , donut donutRadius1 donutRadius2 rvec rθ (θ1 + dθ2) (θ2 + dθ2)
      , donut donutRadius1 donutRadius2 rvec rθ θ1 (θ2 + dθ2)
      ]
    z θ1 θ2 = ( \(Vec3 (_,_,z)) -> z )
                 $ donut 1 1 rvec rθ (θ1 + dθ1 / 2) (θ2 + dθ2 / 2)
    nz θ1 θ2 = ( \(Vec3 (_,_,z)) -> z )
                 $ normalToDonut rvec rθ (θ1 + dθ1 / 2) (θ2 + dθ2 / 2)
    fill nz = 
      let
        nθ    = asin nz
        alpha = sin nθ
      in shade alpha donutColor
      

    dθ1  = 2 * pi / θ1Partitions
    dθ2  = 2 * pi / θ2Partitions
    rθ   = t / period * 2 * pi
    θ1s  = [ x * 2 * pi / θ1Partitions | x <- [0 .. θ1Partitions] ]
    θ2s  = [ x * 2 * pi / θ2Partitions | x <- [0 .. θ2Partitions] ]

main :: IO ()
main = G.animate mainWindow background pictureAtTime
