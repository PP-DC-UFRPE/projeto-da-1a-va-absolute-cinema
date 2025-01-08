module Dados where
import Tipos
import Data.IORef
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

-- Caminho do arquivo de filmes e sessões
filmesFilePath :: FilePath
filmesFilePath = "./BaseDados/filmes.txt"

-- Leitura do arquivo de dados Filmes.txt
instance Read Filme where
    readsPrec _ str = 
        [(Filme titulo generos duracao sinopse, rest4)] where
        (titulo, rest1) = span (/= ';') (drop 6 str) 
        (generosStr, rest2) = span (/= ';') (tail rest1)
        generos = split ',' generosStr
        (duracaoStr, rest3) = span (/= ';') (tail rest2)
        duracao = read duracaoStr :: Int
        (sinopse, rest4) = span (/= '\n') (tail rest3)

-- Função auxiliar para dividir strings
split :: Char -> String -> [String]
split delim str =
    case break (== delim) str of
        (part, "") -> [part]
        (part, _:rest) -> part : split delim rest

-- Função genérica para carregar dados de um arquivo
carregar :: Read a => FilePath -> IO [a]
carregar filePath = do
    existe <- doesFileExist filePath
    if not existe
        then do
            putStrLn $ "Arquivo " ++ filePath ++ " não encontrado. Retornando lista vazia."
            return [] -- Retorna lista vazia se o arquivo não existe
        else do
            conteudo <- readFile filePath
            let dados = mapM readMaybe (lines conteudo)
            case dados of
                Just lista -> return lista
                Nothing -> error $ "Erro ao ler os dados do arquivo: " ++ filePath

-- Função genérica para salvar dados no arquivo
salvar :: Show a => FilePath -> [a] -> IO ()
salvar filePath dados = do
    let conteudo = unlines $ map show dados
    writeFile filePath conteudo

-- Carregar filmes
carregarFilmes :: IO [Filme]
carregarFilmes = carregar filmesFilePath

-- Salvar filmes
salvarFilmes :: [Filme] -> IO ()
salvarFilmes filmes = salvar filmesFilePath filmes

-- Inicialização do sistema com filmes e sessões
inicialSistema :: IO Sistema
inicialSistema = do
    filmes <- carregarFilmes
    return ([], filmes, [], []) -- Inicializa com listas vazias para Cliente, Sessão e Pedido

-- Inicialização do sistema usando IORef
iniciarSistema :: IO (IORef Sistema)
iniciarSistema = do
    sistema <- inicialSistema
    newIORef sistema