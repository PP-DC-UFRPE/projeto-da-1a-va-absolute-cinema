module Utils where

import Tipos
import Data.List (intercalate)

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


formatarCliente :: Cliente -> String
formatarCliente (Cliente nome cpf idade ocupacao) =
    nome ++ ";" ++ cpf ++ ";" ++ show idade ++ ";" ++ show ocupacao

formatarFilme :: Filme -> String
formatarFilme (Filme id titulo genero duracao sinopse) =
    show id ++ ";" ++ titulo ++ ";" ++ formatarGenero genero ++ ";" ++ show duracao ++ ";" ++ sinopse

formatarGenero :: Genero -> String
formatarGenero genero = "[" ++ unwords (map (\g -> g ++ ",") (init genero)) ++ last genero ++ "]"


formatarSessao :: Sessao -> String
formatarSessao (Sessao id (Filme _ titulo _ _ _) horario dia tipoSessao is3D sala assentos) =
    show id ++ ";" ++ titulo ++ ";" ++ formatarHorario horario ++ ";" ++ formatarDia dia ++ ";"
        ++ formatarTipo tipoSessao ++ ";" ++ show is3D ++ ";" ++ show sala ++ ";" ++ formatarAssentos assentos
-- Formata um único assento
formatarAssento :: Assento -> String
formatarAssento (fileira, numero, ocupado) =
    "[" ++ [fileira] ++ "," ++ show numero ++ "," ++ show ocupado ++ "]"
    -- Formata os assentos no formato esperado
formatarAssentos :: [Assento] -> String
formatarAssentos assentos = "[" ++ intercalate "," (map formatarAssento assentos) ++ "]"

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