{- HLINT ignore "Use concatMap" -}

module Futoshiki where
import Distribution.Simple.Command (OptDescr(BoolOpt))

-- Bidimensional Matrix --
type Matrix a = [[a]]

-- Relation between cells --
data Relation = Ind | RGT | RLT
  deriving (Eq, Read, Show)

-- Futoshiki definition --
data Futoshiki = Futoshiki {
  size  :: Int,
  cells :: Matrix Int,
  hRel  :: Matrix Relation,                     -- Este nos da la relación horizontal
  vRel  :: Matrix Relation                      -- Este nos da la relación vertical
}
  deriving (Eq, Read, Show)

-- Función de backtracking genérica --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    esSol          succ          n
  | esSol n                           = [n]     --Nodo es solución, me da la lista de nodos solución del nodo
  | otherwise                         = 
        concat (map (bt esSol succ) (succ n))   --Si nodo no es solución, le aplica la función bt a cada hijo (succ n) con map


-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]              -- Matrix Int = [[Int]]
solve f = map cells (bt esSolucion sucesores f) -- Obtiene cells de las matrices solución

-- Función isSolution --
esSolucion :: Futoshiki -> Bool                 -- Desde un nodo Futoshiki obtener True si es solución, false en caso contrario
esSolucion f = not (any (elem 0) (cells f))     -- ¿Algún 0 en cells de f? Si hay, esSolucion es false

-- Función makeSucesors --
sucesores :: Futoshiki -> [Futoshiki]           -- Desde un nodo Futoshiki generamos una lista con nodo con posible solución
sucesores f = case casillaVacia (cells f) of    -- Encuentra la primera casilla con cero de las celdas de f
    Nothing -> []                               -- No hay ceros, no se generan más sucesores (hijos)
    Just (r,c) -> [ 
                  f { cells = actualizaMatriz (cells f)  
                  r c v} | v <- [1..(size f)] , 
                  isValid f r c v 
                  ]

-- Función isValid --
esValida :: Futoshiki -> Int -> Int -> Int -> Bool


-- Evaluación perezosa -- (No sé para que es...)
checkSolution :: Futoshiki -> Sol -> Bool
checkSolution f None       = (solve f) == []
checkSolution f (Single m) = (solve f) == [m]
checkSolution f Multiple   = checkMultiple (solve f)
    where checkMultiple :: [Matrix Int] -> Bool
          checkMultiple (x:y:zs) = True
          checkMultiple _        = False 
