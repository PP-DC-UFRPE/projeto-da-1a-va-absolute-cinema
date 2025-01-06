module Main where

import Exibir_filmes (exibicao)
import Compra_de_ingresso (visualizarIngressos, compra)
import Tipos (Sistema)
import Dados (iniciarSistema)
import Data.IORef
import System.IO

-- Função que inicializa o programa
main :: IO ()
main = do
    sistemaRef <- iniciarSistema
    loop sistemaRef

-- Loop principal do programa
loop :: IORef Sistema -> IO ()
loop sistemaRef = do
    putStrLn "Menu do cinema"
    putStrLn "1) Exibir filmes disponíveis"
    putStrLn "2) Cadastro de usuário"
    putStrLn "3) Comprar ingresso"
    putStrLn "4) Visualizar ingressos comprados"
    putStrLn "0) Sair"
    putStr "Input: "
    hFlush stdout
    input <- getLine
    putStrLn ""
    casos input sistemaRef

-- Processa a escolha do usuário no menu e chama a função correspondente
casos :: String -> IORef Sistema -> IO ()
casos input sistemaRef = case input of
    "1" -> do
        exibicao sistemaRef  -- Exibe filmes disponíveis
        loop sistemaRef
    "2" -> do
        loop sistemaRef  -- volta ao menu (por enquanto)
    "3" -> do
        compra sistemaRef  -- compra de ingresso
        loop sistemaRef
    "4" -> do
        visualizarIngressos sistemaRef  -- Exibe ingressos comprados
        loop sistemaRef
    "0" -> do
        putStrLn "Saindo do programa"  -- Encerra o programa
    _   -> do
        putStrLn "Opção inválida"  -- Caso o usuário digite uma opção inválida
        loop sistemaRef
