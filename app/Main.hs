import Data.List (sortBy)

-- Define o tipo da posição
type Pos = (Int, Int)
type Path = [Pos]

-- | Função que extrai informações de uma linha do arquivo
parseLine :: String -> Maybe (Int, Int, Pos)
parseLine line =
    case map read (words line) of
        [rows, cols, startRow, startCol] -> Just (rows, cols, (startRow, startCol))
        _ -> Nothing

-- | Função que imprime na tela as variáveis do tabuleiro
printVars :: Int -> Int -> Pos -> IO ()
printVars rows cols startPos = do
    putStrLn $ "\nTabuleiro: " ++ show rows ++ "x" ++ show cols
    putStrLn $ "Cavalo começa em: " ++ show startPos

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

-- | Processa cada linha do arquivo e imprime resultado
processLine :: String -> IO ()
processLine line =
    case parseLine line of
        Just (rows, cols, startPos) -> do
            printVars rows cols startPos
            case knightTour rows cols startPos [startPos] of
                Just path -> putStrLn $ "caminho encontrado: " ++ show path
                Nothing -> putStrLn "Nenhum caminho encontrado"
        Nothing -> putStrLn $ "Linha inválida: " ++ line

-- | Função principal
main :: IO ()
main = do
    content <- readFile "entrada.txt"
    let linhas = lines content
    mapM_ processLine linhas
