module Main where
import Exibir_filmes (exibicao)
import Compra_de_ingresso
import System.IO -- usem quando precisar do hFlush

main :: IO()
main = do
    putStrLn "Menu do cinema"
    putStrLn "1) Exibir filmes disponíveis"
    putStrLn "2) Cadastro de usuário"
    putStrLn "3) Comprar ingresso"
    putStrLn "0) Sair"
    putStr "Input: "
    hFlush stdout -- único jeito de consertar pois não há outro modo de forçar-
                  -- o buffer de texto de um CLI a printar o texto guardado no buffer-
                  -- de texto. O texto sem newline (\n) fica em repouso no buffer até-
                  -- encontrar outro \n, o que acontece quando a gente aperta o enter no input.-
                  -- hFlush força o que está guardado no buffer para fora
    input <- getLine
    casos input
   
casos :: String -> IO ()
casos input = case input of
    "1" -> do 
        exibicao
        main
    "2" -> do 
        main
    "3" -> do 
        compra
        main
    "0" -> do
        putStrLn "Saindo do programa"
    _   -> do
        putStrLn ""
        main

