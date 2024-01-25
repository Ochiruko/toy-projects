module Main where

import Graphics.Gloss

mainWindow :: Display
mainWindow = InWindow "Main Window" (400, 400) (800, 600)

background :: Color
background =  white

clockHand :: Float -> Float-> Picture
clockHand radius theta = Line [(0, 0), endPoint]
  where
    endPoint = (radius * sin theta, radius * cos theta)

rps :: Float
fps :: Float
clockHandRadius :: Float
rps = 1/5
fps = 30
clockHandRadius = 80

pictureAtTime :: Float -> Picture
pictureAtTime t = clockHand clockHandRadius theta
  where 
    nframes = fromInteger . floor $ (t * fps)
    rpf     = rps / fps
    theta   = nframes * rpf * 2 * pi

main :: IO ()
main = animate mainWindow background pictureAtTime