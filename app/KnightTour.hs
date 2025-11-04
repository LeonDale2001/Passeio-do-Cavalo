module KnightTour
  ( Pos
  , Path
  , knightMoves
  , knightTourOpen
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

-- | Verifica se o caminho é aberto (última posição não pode voltar ao início)
isOpenPath :: Path -> Pos -> Int -> Int -> Bool
isOpenPath path startPos rows cols =
    not (startPos `elem` knightMoves (last path) rows cols)

-- | Knight Tour que retorna somente caminhos abertos
knightTourOpen :: Int -> Int -> Pos -> Path -> Maybe Path
knightTourOpen rows cols current visited
    | length visited == rows * cols =
        if isOpenPath visited (head visited) rows cols
           then Just visited  -- caminho aberto encontrado
           else Nothing       -- caminho fechado, ignora
    | otherwise = tryNext (warnsdorffSort nextMoves visited rows cols)
  where
    nextMoves = filter (`notElem` visited) (knightMoves current rows cols)
    tryNext [] = Nothing
    tryNext (m:ms) =
        case knightTourOpen rows cols m (visited ++ [m]) of
            Just path -> Just path
            Nothing -> tryNext ms
