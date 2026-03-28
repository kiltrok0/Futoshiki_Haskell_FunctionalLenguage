
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
  | esSol n                           = [n]     --Si nodo es solución, me da la lista de nodos solución a las que se llega de ese nodo
  | otherwise                         = 
        concat (map (bt esSol succ) (succ n))   --Si nodo no es solución, le aplica la función bt (bt esSol succ) a cada hijo (succ n) con map


-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]              -- Matrix Int = [[Int]]
solve f = map cells (bt esSolucion sucesores f) -- Obtiene cells de las matrices solución

-- Función esSolucion --
esSolucion :: Futoshiki -> Bool                 -- Desde un nodo Futoshiki obtener True si es solución, false en caso contrario
esSolucion f = not (any (elem 0) (cells f))     -- ¿Algún 0 en cells de f? Si hay, esSolucion es false

-- Función sucesores --
sucesores :: Futoshiki -> [Futoshiki]           -- Desde un nodo Futoshiki generamos una lista con nodos con posible solución
sucesores f = case casillaVacia (cells f) of    -- Encuentra la primera casilla con cero de las cells de f
  Nothing -> []                                 -- No hay ceros (condición case), no se generan más sucesores (hijos)
  Just (r,c) -> [                               -- Si hay cero en fila r (row) y columna c (column), trabajará con esas coordenadas
                f { cells = actualizaMatriz     -- Crea la nueva matriz con el valor n en la posición (r,c)
                (cells f) r c n} |
                n <- [1..(size f)]              -- n toma los números posibles del 1 al tamaño del tablero 
                , esValida f r c n              -- Se comprueba si el valor n es válido para darnos una solución, sino, se descarta esa rama del árbol
                ]

-- Función esValida --
esValida :: Futoshiki -> Int -> Int -> Int -> Bool
esValida f r c n = 
  noRepFila &&                                  -- Verifica si v no se repite en la fila
  noRepColumna &&                               -- Verifica si v no se repite en la columna
  condicionHorizontal &&                        -- Verifica si v cumple relación horizontal 
  condicionVertical                             -- Verifica si v cumple relación vertical 
  where 
      noRepFila = n `notElem` (cells f !! r)    -- !! r nos devuelve la fila r (row), extrae la lista dentro de la lista. Se comprueba que no exista el elemento n en la fila.

actualizarMatriz :: Futoshiki -> 

