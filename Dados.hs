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

---- Funções de Mapeamento dos objetos
-- Função para carregar clientes de um arquivo
parseCliente :: String -> Cliente
parseCliente linha = Cliente nome cpf idade ocupacao
    where
        [nome, cpf, idadeStr, ocupacaoStr] = split ';' linha
        idade = read idadeStr
        ocupacao = case ocupacaoStr of
            "Estudante" -> Estudante
            "Professor" -> Professor
            "Outras"    -> Outras
            _           -> error "Ocupação inválida no arquivo"

-- Função para mapear uma linha de texto em um objeto do tipo Filme
-- Cada linha segue o formato: "Titulo;[Genero1,Genero2];Duracao;Sinopse"
parseFilme :: String -> Filme
parseFilme linha = Filme id titulo genero duracao sinopse
    where
        [idStr, titulo, generoStr, duracaoStr, sinopse] = split ';' linha
        id = read idStr
        genero = parseGenero generoStr
        duracao = read duracaoStr

-- Função para mapear uma linha de texto em uma Sessão
-- A linha contém informações do filme, horário, tipo de sessão, 3D, número da sala e assentos
parseSessao :: [Filme] -> String -> Maybe Sessao
parseSessao filmes linha =
    case (filme, tipoSessao) of
        (Just f, Just ts) -> Just $ Sessao id f horario dia ts is3D sala assentos
        _ -> Nothing
    where 
        [idStr, titulo, horarioStr, diaStr, tipoStr, is3DStr, salaStr, assentosStr] = split ';' linha
        id = read idStr
        filme = find ((== titulo) . getTitulo) filmes
        horario = parseHorario horarioStr
        dia = parseDia diaStr
        tipoSessao = readMaybe tipoStr
        is3D = read is3DStr
        sala = read salaStr
        assentos = parseAssentos assentosStr

---- Funções para carregar dados
-- Função para carregar clientes de um arquivo
carregarClientes :: FilePath -> IO [Cliente]
carregarClientes caminho = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else do
            conteudo <- readFile caminho
            return $ map parseCliente (lines conteudo)

-- Função para carregar filmes de um arquivo de texto e convertê-los em uma lista de objetos Filme
carregarFilmes :: FilePath -> IO [Filme]
carregarFilmes caminho = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else do
            conteudo <- readFile caminho
            return $ map parseFilme (lines conteudo)

-- Função para carregar sessões de um arquivo e associá-las aos filmes correspondentes
carregarSessoes :: FilePath -> [Filme] -> IO [Sessao]
carregarSessoes caminho filmes = do
    existe <- doesFileExist caminho
    if not existe
        then return []
        else do
            conteudo <- readFile caminho
            return $ mapMaybe (parseSessao filmes) (lines conteudo)

---- Funções para salvar dados em arquivo .txt
-- Função para salvar clientes em um arquivo
salvarClientes :: [Cliente] -> IO ()
salvarClientes clientes = do
    let caminho = "./BancoDados/clientes.txt"
        conteudo = unlines $ map formatarCliente clientes
    writeFile caminho conteudo

-- Função para salvar filmes em um arquivo
salvarFilmes :: [Filme] -> IO ()
salvarFilmes filmes = do
    let caminho = "./BancoDados/filmes.txt"
        conteudo = unlines $ map formatarFilme filmes
    writeFile caminho conteudo

-- Função para salvar sessões em um arquivo
salvarSessoes :: [Sessao] -> IO ()
salvarSessoes sessoes = do
    let caminho = "./BancoDados/sessoes.txt"
        conteudo = unlines $ map formatarSessao sessoes
    writeFile caminho conteudo

-- Atualizar a função salvarSistema para usar as funções acima
salvarSistema :: IORef Sistema -> IO ()
salvarSistema sistemaRef = do
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, _) = sistema
    salvarClientes clientes
    salvarFilmes filmes
    salvarSessoes sessoes
    putStrLn "Sistema salvo com sucesso!"

---- Função para inicializar o sistema carregando filmes e sessões
inicialSistema :: IO Sistema
inicialSistema = do
    clientes <- carregarClientes "./BancoDados/clientes.txt"
    filmes <- carregarFilmes "./BancoDados/filmes.txt"
    sessoes <- carregarSessoes "./BancoDados/sessoes.txt" filmes
    return (clientes, filmes, sessoes, []) -- Inicializa com listas vazias para Cliente e Pedido

-- Função para criar um sistema com IORef para manipulação do estado
iniciarSistema :: IO (IORef Sistema)
iniciarSistema = do
    sistema <- inicialSistema
    newIORef sistema