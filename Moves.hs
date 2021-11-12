module Moves where

import FuncAux
import MapGenerator

data Move = LeftClick | RightClick deriving (Read,Show,Eq)

move :: Move -> (Int,Int) -> Map -> Map
move x (l,c) map = if ePMatV (l,c) map then movValid x (l,c) (enPMat (l,c) map) map else map


movValid :: Move -> (Int,Int) -> Cell -> Map -> Map
movValid x (l,c) (_,Revealed) map = map
movValid LeftClick (l,c) (_,Flagged) map = map
movValid RightClick (l,c) cell map = flag (l,c) cell map
movValid LeftClick (l,c) cell map = rev (l,c) cell map


flag :: (Int,Int) -> Cell -> Map -> Map
flag (l,c) (x,Flagged) map = atPMat (l,c) (x,Hidden) map
flag (l,c) (x,Hidden) map = atPMat (l,c) (x,Flagged) map


rev :: (Int,Int) -> Cell -> Map -> Map
rev (l,c) (Bomb,stat) map = atPMat (l,c) (Bomb,Revealed) map
rev (l,c) (Square 0,stat) map = arndRev 1 (l,c) (atPMat (l,c) (Square 0,Revealed) map)
rev (l,c) (Square n,stat) map = atPMat (l,c) (Square n,Revealed) map



arndRev :: Int -> (Int,Int) -> Map -> Map
arndRev 9 (l,c) map = map
arndRev x (l,c) map | x == 1 && ePMatV (l+1,c) map = arndRev (x+1) (l,c) (move LeftClick (l+1,c) map)   
                    | x == 2 && ePMatV (l+1,c+1) map = arndRev (x+1) (l,c) (move LeftClick (l+1,c+1) map)   
                    | x == 3 && ePMatV (l,c+1) map = arndRev (x+1) (l,c) (move LeftClick (l,c+1) map)   
                    | x == 4 && ePMatV (l-1,c+1) map = arndRev (x+1) (l,c) (move LeftClick (l-1,c+1) map)   
                    | x == 5 && ePMatV (l-1,c) map = arndRev (x+1) (l,c) (move LeftClick (l-1,c) map)   
                    | x == 6 && ePMatV (l-1,c-1) map = arndRev (x+1) (l,c) (move LeftClick (l-1,c-1) map)   
                    | x == 7 && ePMatV (l,c-1) map = arndRev (x+1) (l,c) (move LeftClick (l,c-1) map)   
                    | x == 8 && ePMatV (l+1,c-1) map = arndRev (x+1) (l,c) (move LeftClick (l+1,c-1) map)
                    | otherwise = arndRev (x+1) (l,c) map   
