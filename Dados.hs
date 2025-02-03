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

---- Funções para carregar dados
-- Função para carregar clientes de um arquivo
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

-- Função para carregar filmes de um arquivo de texto e convertê-los em uma lista de objetos Filme
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

-- Função para carregar sessões de um arquivo e associá-las aos filmes correspondentes
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

-- Função para carregar Pedidos de um arquivo e associá-las aos Cliente e Sessão correspondentes
carregarPedidos :: FilePath -> [Cliente] -> [Sessao] -> IO [Pedido]
carregarPedidos caminho clientes sessoes = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else withFile caminho ReadMode $ \handle -> do
            conteudo <- hGetContents handle
            let pedidos = mapMaybe (parsePedido clientes sessoes) (lines conteudo)
            evaluate (length pedidos)
            return pedidos

---- Funções para salvar dados em arquivo .txt
salvarClientes :: [Cliente] -> IO ()
salvarClientes clientes = do
    let caminho = "./BancoDados/clientes.txt"
        conteudo = unlines $ map formatarCliente clientes
    writeFile caminho conteudo

salvarFilmes :: [Filme] -> IO ()
salvarFilmes filmes = do
    let caminho = "./BancoDados/filmes.txt"
        conteudo = unlines $ map formatarFilme filmes
    writeFile caminho conteudo

salvarSessoes :: [Sessao] -> IO ()
salvarSessoes sessoes = do
    let caminho = "./BancoDados/sessoes.txt"
        conteudo = unlines $ map formatarSessao sessoes
    writeFile caminho conteudo

salvarPedidos :: [Pedido] -> IO ()
salvarPedidos pedidos = do
    let caminho = "./BancoDados/pedidos.txt"
        conteudo = unlines $ map formatarPedido pedidos
    writeFile caminho conteudo

-- Atualizar a função salvarSistema para usar as funções acima
salvarSistema :: IORef Sistema -> IO ()
salvarSistema sistemaRef = do
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, pedidos) = sistema
    salvarClientes clientes
    salvarFilmes filmes
    salvarSessoes sessoes
    salvarPedidos pedidos
    putStrLn "Sistema salvo com sucesso!"

---- Função para inicializar o sistema carregando filmes e sessões
inicialSistema :: IO Sistema
inicialSistema = do
    clientes <- carregarClientes "./BancoDados/clientes.txt"
    filmes <- carregarFilmes "./BancoDados/filmes.txt"
    sessoes <- carregarSessoes "./BancoDados/sessoes.txt" filmes
    pedidos <- carregarPedidos "./BancoDados/pedidos.txt" clientes sessoes
    return (clientes, filmes, sessoes, pedidos)

-- Função para criar um sistema com IORef para manipulação do estado
iniciarSistema :: IO (IORef Sistema)
iniciarSistema = do
    sistema <- inicialSistema
    newIORef sistema