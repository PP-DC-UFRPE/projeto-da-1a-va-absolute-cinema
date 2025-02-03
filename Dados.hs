module Dados where

import Tipos
import Utils
import Data.IORef
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Control.Monad (when)
import System.IO (withFile, hGetContents, IOMode(ReadMode))
import Control.Exception (evaluate)

-- Carrega a lista de clientes a partir de um arquivo.
-- Retorna uma lista vazia se o arquivo não existir.
carregarClientes :: FilePath -> IO [Cliente]
carregarClientes caminho = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else withFile caminho ReadMode $ \handle -> do
            conteudo <- hGetContents handle
            let clientes = map parseCliente (lines conteudo)
            -- Força a avaliação completa da lista de clientes
            evaluate (length clientes)
            return clientes

-- Carrega a lista de filmes a partir de um arquivo.
-- Retorna uma lista vazia se o arquivo não existir.
carregarFilmes :: FilePath -> IO [Filme]
carregarFilmes caminho = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        -- Obs: Como estavamos com problema em repalação ao salvar os aquivos, solicitamos ajuda da IA que nos deus a solução de uso do withFile ao inves do writeFile, por força o arquivo a se fechado apos o uso. Dessa forma não ocorreu mais o erro de leitura.
        else withFile caminho ReadMode $ \handle -> do
            conteudo <- hGetContents handle
            let filmes = map parseFilme (lines conteudo)
            evaluate (length filmes)
            return filmes

-- Carrega a lista de sessões a partir de um arquivo e as associa aos filmes correspondentes.
-- Retorna uma lista vazia se o arquivo não existir.
carregarSessoes :: FilePath -> [Filme] -> IO [Sessao]
carregarSessoes caminho filmes = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else withFile caminho ReadMode $ \handle -> do
            conteudo <- hGetContents handle
            let sessoes = mapMaybe (parseSessao filmes) (lines conteudo)
            evaluate (length sessoes)
            return sessoes

-- Carrega a lista de pedidos a partir de um arquivo e os associa aos clientes e sessões correspondentes.
-- Retorna uma lista vazia se o arquivo não existir.
carregarPedidos :: FilePath -> [Cliente] -> [Sessao] -> IO [Pedido]
carregarPedidos caminho clientes sessoes = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else withFile caminho ReadMode $ \handle -> do
            conteudo <- hGetContents handle
            let pedidos = mapMaybe (parsePedido clientes sessoes) (lines conteudo)
            putStrLn $ "Pedidos: " ++ show pedidos
            evaluate (length pedidos)
            return pedidos

-- Salva a lista de clientes em um arquivo.
salvarClientes :: [Cliente] -> IO ()
salvarClientes clientes = do
    let caminho = "./BancoDados/clientes.txt"
        conteudo = unlines $ map formatarCliente clientes
    writeFile caminho conteudo

-- Salva a lista de filmes em um arquivo.
salvarFilmes :: [Filme] -> IO ()
salvarFilmes filmes = do
    let caminho = "./BancoDados/filmes.txt"
        conteudo = unlines $ map formatarFilme filmes
    writeFile caminho conteudo

-- Salva a lista de sessões em um arquivo.
salvarSessoes :: [Sessao] -> IO ()
salvarSessoes sessoes = do
    let caminho = "./BancoDados/sessoes.txt"
        conteudo = unlines $ map formatarSessao sessoes
    writeFile caminho conteudo

-- Salva a lista de pedidos em um arquivo.
salvarPedidos :: [Pedido] -> IO ()
salvarPedidos pedidos = do
    let caminho = "./BancoDados/pedidos.txt"
        conteudo = unlines $ map formatarPedido pedidos
    writeFile caminho conteudo

-- Salva todo o sistema (clientes, filmes, sessões e pedidos) em arquivos.
-- Exibe uma mensagem de sucesso após a conclusão.
salvarSistema :: IORef Sistema -> IO ()
salvarSistema sistemaRef = do
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, pedidos) = sistema
    salvarClientes clientes
    salvarFilmes filmes
    salvarSessoes sessoes
    salvarPedidos pedidos
    putStrLn "Sistema salvo com sucesso!"

-- Função para inicializar o sistema carregando dados de arquivos.
-- Retorna uma tupla com as listas de clientes, filmes, sessões e pedidos.
inicialSistema :: IO Sistema
inicialSistema = do
    clientes <- carregarClientes "./BancoDados/clientes.txt"
    filmes <- carregarFilmes "./BancoDados/filmes.txt"
    sessoes <- carregarSessoes "./BancoDados/sessoes.txt" filmes
    pedidos <- carregarPedidos "./BancoDados/pedidos.txt" clientes sessoes
    return (clientes, filmes, sessoes, pedidos)

-- Função para criar um sistema com IORef, permitindo a manipulação do estado.
-- Retorna uma referência IORef para o sistema carregado.
iniciarSistema :: IO (IORef Sistema)
iniciarSistema = do
    sistema <- inicialSistema
    newIORef sistema