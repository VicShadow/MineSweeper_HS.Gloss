module FuncAux where

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
-- | Verifica se o indice pertence à lista.
eIListV :: Int -> [a] -> Bool
eIListV n [] = False
eIListV n (h:t) | n < 0 = False
                | (n <= length (h:t) - 1) = True
                | otherwise = False



ground :: Double -> Int
ground x = fromIntegral (floor x)
                           
int2double :: Int -> Double
int2double x = fromIntegral x
                        
rtf :: (Fractional b, Real a) => a -> b
rtf x = realToFrac x 




type DimensaoMatriz = (Int,Int)
type PosicaoMatriz = (Int,Int)
type Matriz a = [[a]]

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
enIList :: Int -> [a] -> a
enIList 0 (h:t) = h
enIList n (h:t) | (n > (length (h:t))) = error "Índice não existente" 
                | otherwise = enIList (n-1) t

-- | Modifica um elemento num dado índice.
atIList :: Int -> a -> [a] -> [a]
atIList 0 x (h:t) = (x:t)
atIList n x (h:t) | eIListV n (h:t) == False = error "Índice não existente"
                  | otherwise = h: (atIList (n-1) x t) 

-- ** Funções sobre matrizes.

-- | Calcula a dimensão de uma matriz.
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimMat :: Matriz a -> DimensaoMatriz
dimMat [[]] = (0,0)
dimMat [] = (0,0)
dimMat ([]:t) = (0,0)
dimMat ((h:t):t1) = (length ((h:t):t1), length (h:t)) 

-- | Verifica se a posição pertence à matriz.
ePMatV :: PosicaoMatriz -> Matriz a -> Bool 
ePMatV (a,b) x | ((a >= 0 && a < l1) && (b>=0 && b< l2)) = True
               | otherwise = False
            where (l1,l2) = dimMat x

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
enPMat :: PosicaoMatriz -> Matriz a -> a
enPMat (a,b) ((h:t):t1) = if ePMatV (a,b) ((h:t):t1) == True 
                                            then enIList b (enIList a ((h:t):t1))
                                            else error "Posição Inexistente"
-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atPMat :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atPMat (l,c) x [] = []
atPMat (0,c) x (l:ls) = (atIList c x l) : ls 
atPMat (l,c) x (l1:ls) = l1 : atPMat (l-1,c) x ls