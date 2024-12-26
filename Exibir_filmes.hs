module Exibir_filmes
(exibicao) where
import Tipos
import Dados

exibicao :: IO()
exibicao = do
    putStrLn "Esses são os filmes disponíveis hoje:"
    printarFilmesESessoes sistema
    
