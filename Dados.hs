module Dados where
import Tipos

filmes = [Filme "Tubarao" [] 124 "", Filme "Avatar" [] 164 "", Filme "Jumanji" [] 104 ""]
sessoes = [Sessao (filmes !! 0) (12, 40) Legendado False 18 [], Sessao (filmes !! 0) (14, 20) Dublado False 9 []]
sistema = ([], filmes, sessoes, [])

