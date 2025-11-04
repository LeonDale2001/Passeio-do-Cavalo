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

-- | Processa cada linha do arquivo e imprime o resultado do Knight Tour
processLine :: String -> IO ()
processLine line =
    case parseLine line of
        Just (rows, cols, startPos) -> do
            printVars rows cols startPos
            case knightTour rows cols startPos [startPos] of
                Just path -> putStrLn $ "caminho encontrado: " ++ show path
                Nothing -> putStrLn "Nenhum caminho encontrado"
        Nothing -> putStrLn $ "Linha inválida: " ++ line
