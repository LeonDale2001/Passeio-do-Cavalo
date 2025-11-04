-- Define o tipo da posição
type Pos = (Int, Int)

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

-- | Função que processa cada linha do arquivo
processLine :: String -> IO ()
processLine line =
    case parseLine line of
        Just (rows, cols, startPos) -> printVars rows cols startPos
        Nothing -> putStrLn $ "Linha inválida: " ++ line

-- | Função principal
main :: IO ()
main = do
    content <- readFile "entrada.txt"
    let linhas = lines content
    mapM_ processLine linhas
