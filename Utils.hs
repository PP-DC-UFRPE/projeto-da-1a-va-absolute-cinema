module Utils where

import Tipos
import Data.List (find)
import Data.List (intercalate)
import Text.Read (readMaybe)

--- Funções de Mapeamento dos objetos
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

-- Função para mapear uma linha de texto em um Pedido
parsePedido :: [Cliente] -> [Sessao] -> String -> Maybe Pedido
parsePedido clientes sessoes linha = 
    case split ';' linha of
        [idStr, cpfCliente, idSessaoStr, ingressosStr, valorStr] -> do
            idPedido <- readMaybe idStr
            cliente <- find (\c -> getCpf c == cpfCliente) clientes
            sessaoId <- readMaybe idSessaoStr
            sessao <- find (\s -> getIdSessao s == sessaoId) sessoes
            ingressos <- parseIngressos ingressosStr
            valor <- readMaybe valorStr
            return $ Ped idPedido cliente sessao ingressos valor
        _ -> Nothing

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
formatarSessao (Sessao id (Filme idFilme _ _ _ _) horario dia tipoSessao is3D sala assentos) =
    show id ++ ";" ++ show idFilme ++ ";" ++ formatarHorario horario ++ ";" ++ formatarDia dia ++ ";"
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

-- Função para mapear a string de ingressos em uma lista de Ingresso
parseIngressos :: String -> Maybe [Ingresso]
parseIngressos str = 
    let cleanStr = filter (`notElem` "[]") str
        ingressosStrs = split ';' cleanStr
    in sequence (map parseIngresso ingressosStrs)

-- Função para mapear uma string em um Ingresso
parseIngresso :: String -> Maybe Ingresso
parseIngresso s = 
    case split ':' s of
        [tipoStr, assentoStr] -> do
            tipo <- parseTipoIngresso tipoStr
            assento <- parseAssento assentoStr
            Just (tipo, assento)
        _ -> Nothing

-- Função para mapear a string de TipoIngresso
parseTipoIngresso :: String -> Maybe TipoIngresso
parseTipoIngresso s =
    case words s of
        ["Inteira", v] -> case readMaybe v of
                            Just f -> Just (Inteira f)
                            _ -> Nothing
        ["Meia"] -> Just Meia
        _ -> Nothing

-- Função para mapear a string de Assento
parseAssento :: String -> Maybe Assento
parseAssento s =
    case split ',' s of
        [fStr, nStr, oStr] -> do
            let f = head fStr  -- Assume que fStr é um único caractere
            n <- readMaybe nStr
            o <- readMaybe oStr
            Just (f, n, o)
        _ -> Nothing

-- Função para formatar um Pedido em uma string
formatarPedido :: Pedido -> String
formatarPedido (Ped id cliente sessao ingressos valor) =
    intercalate ";" [show id, getCpf cliente, show (getIdSessao sessao), formatarIngressos ingressos, show valor]

-- Função para formatar a lista de Ingressos
formatarIngressos :: [Ingresso] -> String
formatarIngressos ingressos = "[" ++ intercalate ";" (map formatarIngresso ingressos) ++ "]"

-- Função para formatar um Ingresso individual
formatarIngresso :: Ingresso -> String
formatarIngresso (tipo, assento) = formatarTipoIngresso tipo ++ ":" ++ formatarAssento assento

-- Função para formatar o TipoIngresso
formatarTipoIngresso :: TipoIngresso -> String
formatarTipoIngresso (Inteira v) = "Inteira " ++ show v
formatarTipoIngresso Meia = "Meia"

-- Função para formatar o Assento
formatarAssentoPedido :: Assento -> String
formatarAssentoPedido (f, n, o) = [f] ++ "," ++ show n ++ "," ++ show o