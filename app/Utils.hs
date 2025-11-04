module Utils
  ( parseLine
  , printVars
  , processLine
  ) where

import KnightTour

-- | Extrai informações de uma linha do arquivo
parseLine :: String -> Maybe (Int, Int, Pos)
parseLine line =
    case map read (words line) of
        [rows, cols, startRow, startCol] -> Just (rows, cols, (startRow, startCol))
        _ -> Nothing

-- | Imprime as informações do tabuleiro e posição inicial
printVars :: Int -> Int -> Pos -> IO ()
printVars rows cols startPos = do
    putStrLn $ "\nTabuleiro: " ++ show rows ++ "x" ++ show cols
    putStrLn $ "Cavalo começa em: " ++ show startPos

-- | Verifica se todas as posições do tabuleiro foram visitadas
verifyFullPath :: Int -> Int -> Path -> IO ()
verifyFullPath rows cols path = do
    let board = [(r, c) | r <- [1..rows], c <- [1..cols]]
        missing = filter (`notElem` path) board
    if null missing
        then putStrLn "Todas as posições foram acessadas."
        else do
            putStrLn "Algumas posições não foram acessadas:"
            print missing

-- | Processa cada linha do arquivo e imprime o resultado do Knight Tour
processLine :: String -> IO ()
processLine line =
    case parseLine line of
        Just (rows, cols, startPos) -> do
            printVars rows cols startPos
            case knightTourOpen rows cols startPos [startPos] of
                Just path -> do 
                    putStrLn $ "caminho encontrado: " ++ show path
                    verifyFullPath rows cols path
                Nothing -> putStrLn "Nenhum caminho encontrado"
        Nothing -> putStrLn $ "Linha inválida: " ++ line
