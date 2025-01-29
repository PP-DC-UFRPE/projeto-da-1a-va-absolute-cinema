module Utils where

import Tipos
import Data.List (intercalate)

-- Função auxiliar para dividir strings em partes com base em um delimitador
split :: Char -> String -> [String]
split delim str =
    case break (== delim) str of
        (part, "") -> [part]
        (part, _:rest) -> part : split delim rest

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
