module KnightTour
  ( Pos
  , Path
  , knightMoves
  , knightTour
  ) where

import Data.List (sortBy)

-- | Tipo da posição e caminho
type Pos = (Int, Int)
type Path = [Pos]

-- | Retorna todos os movimentos válidos do cavalo a partir de uma posição
knightMoves :: Pos -> Int -> Int -> [Pos]
knightMoves (r,c) rows cols = filter inBounds
    [(r+2,c+1),(r+2,c-1),(r-2,c+1),(r-2,c-1)
    ,(r+1,c+2),(r+1,c-2),(r-1,c+2),(r-1,c-2)]
  where
    inBounds (x,y) = x>=1 && x<=rows && y>=1 && y<=cols

-- | Conta quantos movimentos válidos existem a partir de uma posição ignorando visitados
countNextMoves :: Pos -> [Pos] -> Int -> Int -> Int
countNextMoves pos visited rows cols =
    length $ filter (`notElem` visited) (knightMoves pos rows cols)

-- | Ordena movimentos pelo critério de Warnsdorff (menos opções futuras primeiro)
warnsdorffSort :: [Pos] -> [Pos] -> Int -> Int -> [Pos]
warnsdorffSort moves visited rows cols =
    map snd $ sortBy (\(c1,_) (c2,_) -> compare c1 c2)
        [(countNextMoves p visited rows cols, p) | p <- moves]

-- | Knight Tour simples: retorna o primeiro caminho completo encontrado
knightTour :: Int -> Int -> Pos -> Path -> Maybe Path
knightTour rows cols current visited
    | length visited == rows * cols = Just visited
    | otherwise = tryNext (warnsdorffSort nextMoves visited rows cols)
  where
    nextMoves = filter (`notElem` visited) (knightMoves current rows cols)
    tryNext [] = Nothing
    tryNext (m:ms) =
        case knightTour rows cols m (visited ++ [m]) of
            Just path -> Just path
            Nothing -> tryNext ms
