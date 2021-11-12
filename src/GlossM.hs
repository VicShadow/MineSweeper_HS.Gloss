module Main where

import Moves
import MapGenerator
import FuncAux
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game



main :: IO ()
main = do 
       play FullScreen
            (greyN 0.4)
            24
            newGame
            drawBoard
            playerMove
            timeGame

type World = (Memory,Map)
type Memory = (Menu,SeedList,[Picture],Time)
type SeedList = [Int]
type Time = Int

data Menu = Jogo deriving (Eq,Show) 

newGame :: World
newGame = ((Jogo,[],[],0),runMap sqrNbomb (genPer 30 30 10 100)) 

drawBoard :: World -> Picture
drawBoard ((_,_,_,_),map) = Translate (-600) (500) (Scale 1.6 1.6 (Pictures (drawBoardAux map (0,0))))

drawBoardAux :: Map -> (Float,Float) -> [Picture]
drawBoardAux ((h:[]):[]) (x,y) = [Translate x y (drawCell h)]
drawBoardAux ((h:[]):t1) (x,y) = Translate x y (drawCell h) : drawBoardAux t1 (0,y-20)
drawBoardAux ((h:t):t1) (x,y) = Translate x y (drawCell h) : drawBoardAux ((t):t1) (x+20,y)




drawCell :: Cell -> Picture
drawCell (_,Hidden) = Pictures [square,topsquare,outline,outlineXtra]
drawCell (_,Flagged) = Pictures [square,topsquare,outline,outlineXtra,dflag]
drawCell (Bomb,Revealed) = Pictures [square,outline,dbomb]
drawCell (Square n,Revealed) = Pictures [square,outline,dnum n]





dnum :: Int -> Picture
dnum n = Translate (-10) (-10) (dnumaux n)

dnumaux :: Int -> Picture
dnumaux n | 0 == n = Blank
          | otherwise = Translate (15.5) (-5.5) (Scale 0.1 0.1 (Text (show n))) 

dbomb :: Picture
dbomb = Translate (6) (-15.5) (Scale 0.1 0.1 (Text "X"))

dflag :: Picture
dflag = Translate (7) (-14) (Scale 0.07 0.07 (Text "F"))

outlineXtra :: Picture
outlineXtra = Pictures [Line [(5,(-5)),(15,(-5)),(15,(-15)),(5,(-15)),(5,(-5))],outlineAux]

outlineAux :: Picture
outlineAux = Pictures [Line [(0,0),(5,(-5))] , Line [(20,0),(15,(-5))] , Line [(0,-20),(5,(-15))],Line [(20,(-20)),(15,(-15))]]

outline :: Picture
outline = Line [(0,0),(20,0),(20,(-20)),(0,(-20)),(0,0)]

square :: Picture
square = Color (makeColorI 192 192 192 256) (Polygon [(0,0),(20,0),(20,(-20)),(0,(-20))])

topsquare :: Picture
topsquare = Color (makeColorI 220 220 220 256) (Polygon [(5,(-5)),(15,(-5)),(15,(-15)),(5,(-15))])








playerMove :: Event -> World -> World
playerMove (EventKey (MouseButton LeftButton) Down _ (x,y)) ((a,b,c,d),map) = ((a,b,c,d),move LeftClick (floor (-1/32 * y + 15.625),floor (1/32 * x + 18.75)) map)
playerMove (EventKey (MouseButton RightButton) Down _ (x,y)) ((a,b,c,d),map) = ((a,b,c,d),move RightClick (floor (-1/32 * y + 15.625),floor (1/32 * x + 18.75)) map)
playerMove x c = c

timeGame :: Float -> World -> World
timeGame x c = c 