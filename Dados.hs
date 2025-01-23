module Dados where

import Tipos
import Data.IORef
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

-- Função auxiliar para dividir strings em partes com base em um delimitador
split :: Char -> String -> [String]
split delim str =
    case break (== delim) str of
        (part, "") -> [part]
        (part, _:rest) -> part : split delim rest


---- Funções de Mapeamento dos objetos:
-- Função para carregar clientes de um arquivo
parseCliente :: String -> Cliente
parseCliente linha = 
    let partes = split ';' linha
        nome = partes !! 0
        cpf = partes !! 1
        idade = read (partes !! 2) :: Int
        ocupacaoStr = partes !! 3
        ocupacao = case ocupacaoStr of
            "Estudante" -> Estudante
            "Professor" -> Professor
            "Outras"    -> Outras
            _            -> error "Ocupação inválida no arquivo"
    in Cliente nome cpf idade ocupacao

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

-- Função para mapear uma string no formato "HH:MM" para um tipo Horario (int, int)
parseHorario :: String -> Horario
parseHorario str = 
    let [h, m] = map read $ split ':' str
    in (h, m)

-- Função para mapear uma string no formato "DD/MM/AAAA" para um tipo Dia (int, int, int)
parseDia :: String -> Dia
parseDia str = 
    let [d, m, a] = map read $ split '/' str
    in (d, m, a)

-- Função para mapear uma string de assentos para uma lista de Assentos
parseAssentos :: String -> [Assento]
parseAssentos str = 
    let -- Remove os colchetes ao redor da lista de assentos
        limpar = filter (`notElem` "[]") str
        -- Divide por vírgulas, cada grupo de 3 representará um assento
        assentosStrs = split ',' limpar
    in map (\[fileira, numero, ocupado] -> (head fileira, read numero, read ocupado)) (agrupaAssentos assentosStrs)

-- Agrupa em listas de 3 elementos
agrupaAssentos :: [String] -> [[String]]
agrupaAssentos [] = []
agrupaAssentos (a:b:c:xs) = [a, b, c] : agrupaAssentos xs
agrupaAssentos _ = error "Formato inválido de assento."

-- Função para mapear uma linha de texto em uma Sessão
-- Ex: A linha contém informações do filme, horário, tipo de sessão, 3D, número da sala e assentos
parseSessao :: [Filme] -> String -> Maybe Sessao
parseSessao filmes linha = 
    let partes = split ';' linha
        titulo = partes !! 0
        filme = find (\(Filme t _ _ _) -> t == titulo) filmes
        horario = parseHorario $ partes !! 1
        dia = parseDia $ partes !! 2
        tipoSessao = readMaybe (partes !! 3) :: Maybe TipoSessao
        is3D = read (partes !! 4) :: Bool
        sala = read (partes !! 5) :: Int
        assentos = parseAssentos $ partes !! 6
    in case (filme, tipoSessao) of
        (Just f, Just ts) -> Just $ Sessao f horario dia ts is3D sala assentos
        _ -> Nothing


---- Funções para carregar dados:
-- Função para carregar clientes de um arquivo de texto
carregarClientes :: FilePath -> IO [Cliente]
carregarClientes caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
    linhas `seq` return (map parseCliente linhas)

-- Função para carregar filmes de um arquivo de texto 
carregarFilmes :: FilePath -> IO [Filme]
carregarFilmes caminho = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
    return $ map parseFilme linhas

-- Função para carregar sessões de um arquivo e associá-las aos filmes correspondentes
carregarSessoes :: FilePath -> [Filme] -> IO [Sessao]
carregarSessoes caminho filmes = do
    conteudo <- readFile caminho
    let linhas = lines conteudo
    return $ mapMaybe (parseSessao filmes) linhas


---- Funções para salvar dados em arquivo .txt:
-- Função para salvar filmes em um arquivo
salvarFilmes :: [Filme] -> IO ()
salvarFilmes filmes = do
    let caminho = "./BaseDados/filmes.txt"
        conteudo = unlines $ map formatarFilme filmes
    writeFile caminho conteudo
  where
    formatarFilme :: Filme -> String
    formatarFilme (Filme titulo genero duracao sinopse) =
        titulo ++ ";" ++ formatarGenero genero ++ ";" ++ show duracao ++ ";" ++ sinopse

    formatarGenero :: Genero -> String
    formatarGenero genero = "[" ++ unwords (map (\g -> g ++ ",") (init genero)) ++ last genero ++ "]"

-- Função para salvar sessões em um arquivo
salvarSessoes :: [Sessao] -> IO ()
salvarSessoes sessoes = do
    let caminho = "./BaseDados/sessoes.txt"
        conteudo = unlines $ map formatarSessao sessoes
    writeFile caminho conteudo
  where
    formatarSessao :: Sessao -> String
    formatarSessao (Sessao (Filme titulo _ _ _) horario dia tipoSessao is3D sala assentos) =
        titulo ++ ";" ++ formatarHorario horario ++ ";" ++ formatarDia dia ++ ";" 
            ++ formatarTipo tipoSessao ++ ";" ++ show is3D ++ ";" ++ show sala ++ ";" ++ formatarAssentos assentos

    -- Formata o horário no formato HH:MM
    formatarHorario :: Horario -> String
    formatarHorario (h, m) = show h ++ ":" ++ (if m < 10 then "0" ++ show m else show m)

    -- Formata a data no formato DD/MM/AAAA
    formatarDia :: Dia -> String
    formatarDia (d, m, a) = 
        let formatar n = if n < 10 then "0" ++ show n else show n
        in formatar d ++ "/" ++ formatar m ++ "/" ++ show a

    -- Formata o tipo de sessão (Dublado ou Legendado)
    formatarTipo :: TipoSessao -> String
    formatarTipo Dublado = "Dublado"
    formatarTipo Legendado = "Legendado"

    -- Formata os assentos no formato esperado
    formatarAssentos :: [Assento] -> String
    formatarAssentos assentos = "[" ++ intercalate "," (map formatarAssento assentos) ++ "]"

    -- Formata um único assento
    formatarAssento :: Assento -> String
    formatarAssento (fileira, numero, ocupado) =
        "[" ++ [fileira] ++ "," ++ show numero ++ "," ++ show ocupado ++ "]"

-- Função para salvar clientes em um arquivo
salvarClientes :: [Cliente] -> IO ()
salvarClientes clientes = do
    let caminho = "./BaseDados/clientes.txt"
        conteudo = unlines $ map formatarCliente clientes
    writeFile caminho conteudo
  where
    formatarCliente :: Cliente -> String
    formatarCliente (Cliente nome cpf idade ocupacao) =
        nome ++ ";" ++ cpf ++ ";" ++ show idade ++ ";" ++ show ocupacao

-- Função principal para salvar todos os dados (Cliente, Filme, Sessão)
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
    --clientes <- carregarClientes "./BaseDados/clientes.txt"
    filmes <- carregarFilmes "./BaseDados/filmes.txt"
    sessoes <- carregarSessoes "./BaseDados/sessoes.txt" filmes
    return ([], filmes, sessoes, []) -- Inicializa com listas vazias para Cliente e Pedido

---- Função para criar um sistema com IORef para manipulação do estado
iniciarSistema :: IO (IORef Sistema)
iniciarSistema = do
    sistema <- inicialSistema
    newIORef sistema