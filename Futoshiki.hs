{- HLINT ignore "Use concatMap" -}

module Futoshiki where

-- Bidimensional Matrix --
type Matrix a = [[a]]

-- Relation between cells --
data Relation = Ind | RGT | RLT
  deriving (Eq, Read, Show)

-- Futoshiki definition --
data Futoshiki = Futoshiki {
  size  :: Int,
  cells :: Matrix Int,
  hRel  :: Matrix Relation, -- Este nos da la relación horizontal
  vRel  :: Matrix Relation -- Este nos da la relación vertical
}
  deriving (Eq, Read, Show)

-- Función de backtracking genérica --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    esSol          succ          n
  | esSol n                           = [n] --Nodo es solución, me da la lista de nodos solución del nodo
  | otherwise                         = concat (map (bt esSol succ) (succ n)) --Si nodo no es solución, le aplica la función bt a cada hijo (succ n) con map


-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int] -- Matrix Int = [[Int]]
solve f = map cells (bt esSolucion sucesores f) -- Obtiene cells de las matrices solución

-- Función isSolution --
esSolucion :: Futoshiki -> Bool -- Desde un nodo Futoshiki obtener True si es solución, false en caso contrario
esSolucion f = not (any (elem 0) (cells f)) -- ¿Algún 0 en cells de f? Si hay, esSolucion es false

-- Función makeSucesors --
sucesores :: Futoshiki -> [Futoshiki] -- Desde un nodo Futoshiki generamos una lista con nodo con posible solución
sucesores f = case foundFirstEmpty (cells f) of  -- Encuentra la primera casilla con cero de las celdas de f
    Nothing -> [] --No hay ceros, no se generan más sucesores (hijos)
    Just (r,c) -> [ f { cells = updateMatrix (cells f)  r c v} | v <- [1..(size f)] , isValid f r c v ]

-- Función isValid --
isValid :: Futoshiki -> Int -> Int -> Int -> Bool
isValid f r c v = notInRow && notInCol &&  horCompliance && verCompliance
      where
          notInRow = v `notElem` cells f !! r
          notInCol = v `notElem` [cells f !! i !! c | i <- [0..(size f-1)]]

          horCompliance = (c == 0 || verify (hRel f !! r !! (c-1)) (cells f !! r !! (c-1)) v) &&
                          (c == size f-1 || verify (hRel f !! r !! c) v (cells f !! r !! (c+1)))

          verCompliance = (r == 0 || verify (vRel f !! (r-1) !! c) (cells f !! (r-1) !! c) v) &&
                          (r == size f-1 || verify (vRel f !! r !! c) v (cells f !! (r+1) !! c))

          verify rel v1 v2
                | v1 == 0 || v2 == 0 = True
                | rel == RGT         = v1 > v2
                | rel == RLT         = v1 < v2
                | otherwise          = True

-- Función foundFirstEmpty --
foundFirstEmpty :: Matrix Int -> Maybe (Int,Int)
foundFirstEmpty m =
          let empty = [(r,c) | r <- [0..n-1], c <- [0..n-1], m !! r !! c == 0]
              n = length m
          in if null empty then Nothing else Just (head empty)

-- Función foundFirstEmpty
updateMatrix :: Matrix a -> Int -> Int -> a -> Matrix a
updateMatrix m r c v = take r m ++ [newRow] ++ drop (r+1) m
          where
            origRow = m !! r
            newRow = take c origRow ++ [v] ++ drop (c+1) origRow