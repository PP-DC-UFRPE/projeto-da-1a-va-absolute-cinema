module Dados where

import Tipos
import Data.IORef
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Maybe (mapMaybe)

-- Função auxiliar para dividir strings em partes com base em um delimitador
split :: Char -> String -> [String]
split delim str =
    case break (== delim) str of
        (part, "") -> [part]
        (part, _:rest) -> part : split delim rest

-- Função para mapear o campo de gêneros de um filme de uma string para uma lista de strings
-- Exemplo: "[Aventura,Ficção]" -> ["Aventura", "Ficção"]
parseGenero :: String -> [String]
parseGenero str = split ',' (filter (`notElem` "[]") str)

-- Função para mapear uma linha de texto em um objeto do tipo Filme
-- Cada linha segue o formato: "Titulo;[Genero1,Genero2];Duracao;Sinopse"
parseFilme :: String -> Filme
parseFilme linha = 
    let partes = split ';' linha
        titulo = partes !! 0
        genero = parseGenero (partes !! 1)
        duracao = read (partes !! 2) :: Int
        sinopse = partes !! 3
    in Filme titulo genero duracao sinopse

-- Função para carregar filmes de um arquivo de texto e convertê-los em uma lista de objetos Filme
carregarFilmes :: FilePath -> IO [Filme]
carregarFilmes caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
    return $ map parseFilme linhas

-- Função para mapear uma string no formato "HH:MM" para um tipo Horario (tupla de inteiros)
parseHorario :: String -> Horario
parseHorario str = 
    let [h, m] = map read $ split ':' str
    in (h, m)

-- Função para mapear uma string no formato "DD/MM/AAAA" para um tipo Dia (tupla de inteiros)
parseDia :: String -> Dia
parseDia str = 
    let [d, m, a] = map read $ split '/' str
    in (d, m, a)

-- Função para mapear a string de assentos no formato "[('A',1,False),...]" para uma lista de Assento
parseAssentos :: String -> [Assento]
parseAssentos str =
    let limpar = filter (`notElem` "[]() ") str -- Remove caracteres desnecessários
        assentosStrs = split ',' limpar         -- Divide a string por vírgulas
    in map parseAssento (agrupaAssentos assentosStrs)

-- Divide a lista de strings em grupos de três (Char, Int, Bool)
-- Exemplo: ["A","1","True","B","2","False"] -> [["A","1","True"],["B","2","False"]]
agrupaAssentos :: [String] -> [[String]]
agrupaAssentos [] = []
agrupaAssentos (a:b:c:xs) = [a, b, c] : agrupaAssentos xs
agrupaAssentos _ = error "Formato inválido de assentos."

-- Converte um grupo de três strings em um objeto Assento
-- Exemplo: ["A","1","True"] -> ('A',1,True)
parseAssento :: [String] -> Assento
parseAssento [fileira, numero, ocupado] =
    (head fileira, read numero, read ocupado)
parseAssento _ = error "Formato inválido de assento."

-- Função para mapear uma linha de texto em uma Sessão
-- A linha contém informações do filme, horário, tipo de sessão, 3D, número da sala e assentos
parseSessao :: [Filme] -> String -> Maybe Sessao
parseSessao filmes linha = 
    let partes = split ';' linha
        titulo = partes !! 0
        filme = find (\(Filme t _ _ _) -> t == titulo) filmes
        horario = parseHorario $ partes !! 1
        tipoSessao = readMaybe (partes !! 3) :: Maybe TipoSessao
        is3D = read (partes !! 4) :: Bool
        sala = read (partes !! 5) :: Int
        assentos = parseAssentos $ partes !! 6
    in case (filme, tipoSessao) of
        (Just f, Just ts) -> Just $ Sessao f horario ts is3D sala assentos
        _ -> Nothing

-- Função para carregar sessões de um arquivo e associá-las aos filmes correspondentes
carregarSessoes :: FilePath -> [Filme] -> IO [Sessao]
carregarSessoes caminho filmes = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
    return $ mapMaybe (parseSessao filmes) linhas

-- Função para inicializar o sistema carregando filmes e sessões
inicialSistema :: IO Sistema
inicialSistema = do
    filmes <- carregarFilmes "./BaseDados/filmes.txt"
    sessoes <- carregarSessoes "./BaseDados/sessoes.txt" filmes
    return ([], filmes, sessoes, []) -- Inicializa com listas vazias para Cliente e Pedido

-- Função para criar um sistema com IORef para manipulação do estado
iniciarSistema :: IO (IORef Sistema)
iniciarSistema = do
    sistema <- inicialSistema
    newIORef sistema