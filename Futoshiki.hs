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
  | esSol n                           = [n]
  | otherwise                         = concat (map (bt esSol succ) (succ n))

-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]

