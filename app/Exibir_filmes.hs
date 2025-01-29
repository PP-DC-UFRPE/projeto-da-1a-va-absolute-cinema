module Exibir_filmes (exibicao) where
    
import Tipos
import Dados
import Data.IORef

-- Exibe os filmes disponíveis e suas sessões
exibicao :: IORef Sistema -> IO ()
exibicao sistemaRef = do
    sistema <- readIORef sistemaRef
    putStrLn "Esses são os filmes disponíveis hoje:"
    printarFilmesESessoes sistema
    putStrLn ""
