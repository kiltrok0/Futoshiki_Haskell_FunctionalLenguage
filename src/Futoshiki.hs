
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
                n <- [1 .. (size f)]              -- n toma los números posibles del 1 al tamaño del tablero 
                , esValida f r c n              -- Se comprueba si el valor n es válido para darnos una solución, sino, se descarta esa rama del árbol
                ]

-- Función esValida --
esValida :: Futoshiki -> Int -> Int -> Int -> Bool
esValida f r c n =                              -- Validamos si es correcto meter el número n en la fila y columna (r,c) dentro del tablero Futoshiki f
  noRepFila &&                                  -- Verifica si v no se repite en la fila
  noRepColumna &&                               -- Verifica si v no se repite en la columna
  condicionHorizontal &&                        -- Verifica si v cumple relación horizontal 
  condicionVertical                             -- Verifica si v cumple relación vertical 
  where 
      noRepFila = n `notElem` (cells f !! r)    -- !! r nos devuelve la fila r (row), extrae la lista dentro de la lista. Se comprueba que no exista el elemento n en la fila.
      
      noRepColumna = n `notElem` [cells f !! i !! c | i <- [0 .. (size f - 1)]] 

      condicionHorizontal = (c == 0 || verificar (hRel f !! r !! (c - 1)) (cells f !! r !! (c - 1)) n) &&   -- Si c == 0, no hay vecino izquiero
                            (c == (size f - 1) || verificar (hRel f !! r !! c) n (cells f !! r !! (c + 1))) -- Si c == size f - 1, no hay vecino derecho

      condicionVertical   = (r == 0 || verificar (vRel f !! (r - 1) !! c) (cells f !! (r - 1) !! c) n) &&   -- Si r == 0, no hay vecino superior
                            (r == (size f - 1) || verificar (vRel f !! r !! c) n (cells f !! (r + 1) !! c)) -- Si r == size f - 1, no hay vecino inferior


      -- Función auxiliar verificar -- 
      verificar rel v1 v2                       -- Se verifica si el valor v1 cumple la relación con el valor v2, y viceversa
            | v1 == 0 || v2 == 0 = True
            | rel == RGT         = v1 > v2
            | rel == RLT         = v1 < v2
            | otherwise          = True 

-- Función casillaVacia --

casillaVacia :: Matrix Int -> Maybe (Int,Int)   -- Nos da la coordenaras (r,c) del primer 0 que encuentre
casillaVacia m =  let vacios = [(r,c) | r <- [0 .. l - 1], c <- [0 .. l - 1], m !! r !! c == 0]
                      l = length m
                  in if null vacios then Nothing else Just (head vacios)

-- Función actualizarMatriz --