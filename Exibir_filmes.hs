module Exibir_filmes
(exibicao) where
import Tipos

jaws = Filme "Tubarao" ["Suspense", "Comedia", "Ficcao Cientifica", "Drama", "Misterio", "Aventura"] 124 "Um filme de um tubarao" 
s = Sessao jaws (12, 40) Dublado True 2 [('d', 2, False)]

--printar :: Cliente -> String
--printar (Cliente n c i o) = "Nome: " ++ n ++ ", CPF: " ++ c

printar :: Sessao -> String
printar (Sessao f h t i s a) = "Filme: " ++ show f

exibicao :: IO()
exibicao = do
    putStrLn(pegarTituloDaSessao s)
    putStrLn(pegarTitulo jaws)
    putStrLn(printarFilme jaws)
    putStrLn(printar s)
