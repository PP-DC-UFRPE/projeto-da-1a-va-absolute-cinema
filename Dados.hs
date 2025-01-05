module Dados where
import Tipos
import Data.IORef

filmes = [Filme "Tubarao" [] 124 "", Filme "Avatar" [] 164 "", Filme "Jumanji" [] 104 ""]
sessoes = [Sessao (filmes !! 0) (12, 40) Legendado False 18 [(c, i, False) | c <- ['a'..'e'], i <- [1..7]]]
inicialSistema = ([], filmes, sessoes, [])

iniciarSistema :: IO (IORef Sistema)
iniciarSistema = newIORef inicialSistema
