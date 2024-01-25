module Main where

import Graphics.Gloss
import Donut

mainWindow :: Display
mainWindow = InWindow "Main Window" (400, 400) (0, 0)

background :: Color
background =  white

clockHand :: Point -> Float -> Float -> Picture
clockHand center@(cx, cy) radius theta = Line [center, endPoint]
  where
    endPoint = (cx + radius * sin theta, cy + radius * cos theta)

rps :: Float
fps :: Float
clockHandRadius :: Float
rps = 1/5
fps = 30
clockHandRadius = 50

pictureAtTime :: Float -> Picture
pictureAtTime t = clockHand (200, 200) clockHandRadius theta
  where 
    nframes = fromInteger . floor $ (t * fps)
    rpf     = rps / fps
    theta   = nframes * rpf * 2 * pi

main :: IO ()
main = animate mainWindow background pictureAtTime