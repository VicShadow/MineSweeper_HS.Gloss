module MapGenerator where

import System.Random
import FuncAux

data Type = Square Int | Bomb deriving (Read,Show,Eq)
data State = Hidden | Revealed | Flagged deriving (Read,Show,Eq)
type Cell = (Type,State) 
type Map = [[Cell]]

genPer :: Int -> Int -> Int -> Int -> Map
genPer l c per seed | l <= 1 || c <= 1 || (per < 0 && per > 100) = genPer 2 2 0 seed
                    | otherwise = genMap l c per (take (l*c) (randomRs (0,99) (mkStdGen seed))) 
               
genMap :: Int -> Int -> Int -> [Int] -> Map
genMap 0 c per t = []
genMap l c per t = genL c per (take c t) : genMap (l-1) c per (drop c t) 

genL :: Int -> Int -> [Int] -> [Cell]
genL 0 per l = []
genL c per (h:t) | (h+1) <= per = (Bomb,Hidden) : genL (c-1) per t
                 | otherwise = (Square 0,Hidden) : genL (c-1) per t



runMap :: (Cell -> [Cell] -> Cell) -> Map -> Map
runMap func map = runMapAux (length map - 1, length (head map) - 1) (0,0) func map

runMapAux :: (Int,Int) -> (Int,Int) -> (Cell -> [Cell] -> Cell) -> Map -> Map
runMapAux (0,0) (l,c) func map = atPMat (l,c) (func (enPMat (l,c) map) (aroundCell (l,c) map)) map
runMapAux (lx,0) (l,c) func map = runMapAux (lx-1,c) (l+1,0) func (atPMat (l,c) (func (enPMat (l,c) map) (aroundCell (l,c) map)) map)
runMapAux (lx,lc) (l,c) func map = runMapAux (lx,lc-1) (l,c+1) func (atPMat (l,c) (func (enPMat (l,c) map) (aroundCell (l,c) map)) map)
                     

aroundCell :: (Int,Int) -> Map -> [Cell]
aroundCell (l,c) mapa | l == 0               && c == 0                       = arndCellAux 3 5 (l,c) mapa
                      | l == 0               && (c+1) == length (head mapa)  = arndCellAux 5 7 (l,c) mapa
                      | (l+1) == length mapa && c == 0                       = arndCellAux 1 3 (l,c) mapa
                      | (l+1) == length mapa && (c+1) == length (head mapa)  = arndCellAux 1 1 (l,c) mapa ++ arndCellAux 7 8 (l,c) mapa
                      | l == 0                                               = arndCellAux 3 7 (l,c) mapa
                      | c == 0                                               = arndCellAux 1 5 (l,c) mapa 
                      | (l+1) == length mapa                                 = arndCellAux 1 3 (l,c) mapa ++ arndCellAux 7 8 (l,c) mapa
                      | (c+1) == length (head mapa)                          = arndCellAux 1 1 (l,c) mapa ++ arndCellAux 5 8 (l,c) mapa
                      | otherwise                                            = arndCellAux 1 8 (l,c) mapa


arndCellAux :: Int -> Int -> (Int,Int) -> Map -> [Cell]
arndCellAux x y (l,c) mapa | x > y = []
                           | x == 1 = enPMat (l-1,c) mapa   : arndCellAux (x+1) y (l,c) mapa 
                           | x == 2 = enPMat (l-1,c+1) mapa : arndCellAux (x+1) y (l,c) mapa
                           | x == 3 = enPMat (l,c+1) mapa   : arndCellAux (x+1) y (l,c) mapa
                           | x == 4 = enPMat (l+1,c+1) mapa : arndCellAux (x+1) y (l,c) mapa
                           | x == 5 = enPMat (l+1,c) mapa   : arndCellAux (x+1) y (l,c) mapa
                           | x == 6 = enPMat (l+1,c-1) mapa : arndCellAux (x+1) y (l,c) mapa
                           | x == 7 = enPMat (l,c-1) mapa   : arndCellAux (x+1) y (l,c) mapa
                           | x == 8 = enPMat (l-1,c-1) mapa : arndCellAux (x+1) y (l,c) mapa



sqrNbomb :: Cell -> [Cell] -> Cell
sqrNbomb (Bomb,state) l = (Bomb,state)
sqrNbomb (Square n,state) [] = (Square n,state) 
sqrNbomb (Square n,state) ((Bomb,_):t) = sqrNbomb (Square (n+1),state) t 
sqrNbomb (Square n,state) ((Square _,_):t) = sqrNbomb (Square n,state) t 


