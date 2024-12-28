module Compra_de_ingresso where

import Dados
import Tipos
import System.IO


compra :: IO ()
compra = do
    putStrLn "Voce deseja ver a lista de filmes?"
    putStr "Digite 's' se sim e 'n' se não: "
    hFlush stdout
    input <- getChar
    _ <- getLine
    if input == 's' then printarFilmesESessoes sistema else return ()
    putStrLn "Qual filme voce quer assistir?"
    putStr "Digite o numero 'vai de 0 ate n':"
    hFlush stdout
    input <- getLine
    let num = read input :: Int
    printarSessoesPorFilme sessoes (filmes !! num)

    putStr "Digite o numero da sala: "
    hFlush stdout
    input2 <- getLine
    let num2 = read input2 :: Int
    printarSessaoPorNumero num2 sessoes
    putStrLn ""
