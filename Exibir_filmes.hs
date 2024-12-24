module Exibir_filmes
(exibicao) where
import Tipos

jaws = Filme "Jaws" ["Acao"] 90 "hahah" 
s = Sessao jaws (12, 40) Dublado True 2 [('d', 2, False)]

--printar :: Cliente -> String
--printar (Cliente n c i o) = "Nome: " ++ n ++ ", CPF: " ++ c

printar :: Sessao -> String
printar (Sessao f h t i s a) = "Filme: " ++ show f

pegarTitulo :: Sessao -> String
pegarTitulo (Sessao (Filme t _ _ _) _ _ _ _ _ ) = t --acho inteligente criar um arquivo de utilidades com funções desse tipo

exibicao :: IO()
exibicao = do
    putStrLn(pegarTitulo s)
    putStrLn(printar s)
