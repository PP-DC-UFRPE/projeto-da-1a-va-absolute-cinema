module Main where
import Tipos
import Data.IORef
import System.IO
import Dados
import Exibir_filmes
import Compra_de_ingresso


-- Função que inicializa o programa
main :: IO ()
main = do
    sistemaRef <- iniciarSistema
    loop sistemaRef

-- Loop principal do programa
loop :: IORef Sistema -> IO ()
loop sistemaRef = do
    putStrLn "----- Menu do cinema -----"
    putStrLn "1) Exibir filmes disponíveis"
    putStrLn "2) Cadastro de usuário"
    putStrLn "3) Comprar ingresso"
    putStrLn "4) Modo administrador"
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
        salvarSistema sistemaRef
        loop sistemaRef
    "4" -> do
        loginAdmin sistemaRef -- Entra no modo administrador
        loop sistemaRef
    "0" -> do
        salvarSistema sistemaRef
        putStrLn "Saindo do programa"  -- Encerra o programa
    _   -> do
        putStrLn "Opção inválida"  -- Caso o usuário digite uma opção inválida
        loop sistemaRef

loginAdmin :: IORef Sistema -> IO ()
loginAdmin sistemaRef = do
    putStrLn "----- Acesso de administrador -----"
    putStrLn "\nDigite a senha de administrador:"
    putStr "Senha: "
    hFlush stdout
    senha <- getLine
    if senha == senhaAdmin
        then do
            putStrLn "\nAcesso concedido!"
            menuAdmin sistemaRef
        else do
            putStrLn "\nSenha incorreta!"
            loop sistemaRef

menuAdmin :: IORef Sistema -> IO ()
menuAdmin sistemaRef = do
    putStrLn "\n----- Menu do administrador -----"
    putStrLn "1) Sessões"
    putStrLn "2) Filmes"
    putStrLn "3) Clientes"
    putStrLn "4) Pedidos"
    putStrLn "5) Relatórios"
    putStrLn "0) Sair"
    putStr "Input: "
    hFlush stdout
    input <- getLine
    putStrLn ""
    casosAdmin input sistemaRef

casosAdmin :: String -> IORef Sistema -> IO ()
casosAdmin input sistemaRef = case input of
    {-
    "1" -> do
        menuSessoes sistemaRef
    "2" -> do
        menuFilmes sistemaRef
    "3" -> do
        menuClientes sistemaRef
    "4" -> do
        menuPedidos sistemaRef
    "5" -> do
        menuRelatorios sistemaRef
    -}
    "0" -> do
        putStrLn "Saindo do modo administrador"
        loop sistemaRef
    _   -> do
        putStrLn "Opção inválida!"
        menuAdmin sistemaRef
    