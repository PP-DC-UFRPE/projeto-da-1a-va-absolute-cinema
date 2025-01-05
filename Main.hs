module Main where
import Exibir_filmes (exibicao)
import Compra_de_ingresso
import Tipos (Sistema)
import Dados (iniciarSistema)
import Data.IORef
import System.IO

main :: IO ()
main = do
    sistemaRef <- iniciarSistema
    loop sistemaRef

loop :: IORef Sistema -> IO ()
loop sistemaRef = do
    putStrLn "Menu do cinema"
    putStrLn "1) Exibir filmes disponíveis"
    putStrLn "2) Cadastro de usuário"
    putStrLn "3) Comprar ingresso"
    putStrLn "0) Sair"
    putStr "Input: "
    hFlush stdout
    input <- getLine
    putStrLn ""
    casos input sistemaRef

casos :: String -> IORef Sistema -> IO ()
casos input sistemaRef = case input of
    "1" -> do
        exibicao sistemaRef
        loop sistemaRef
    "2" -> do
        loop sistemaRef
    "3" -> do
        compra sistemaRef
        loop sistemaRef
    "0" -> do
        putStrLn "Saindo do programa"
    _   -> do
        putStrLn "Opção inválida"
        loop sistemaRef
