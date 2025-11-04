module Main where

import Utils

main :: IO ()
main = do
    content <- readFile "entrada.txt"
    let linhas = lines content
    mapM_ processLine linhas
