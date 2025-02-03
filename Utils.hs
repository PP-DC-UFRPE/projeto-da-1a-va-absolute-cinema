module Utils where

import Tipos
import Data.List (find)
import Data.List (intercalate)
import Text.Read (readMaybe)

-- Função auxiliar para dividir strings em partes com base em um delimitador.
-- Exemplo: split ';' "a;b;c" -> ["a", "b", "c"]
split :: Char -> String -> [String]
split delim str =
    case break (== delim) str of
        (part, "") -> [part]
        (part, _:rest) -> part : split delim rest

--- Funções de mapeamento de strings para objetos

-- Converte uma linha de texto em um objeto do tipo Cliente.
-- Formato esperado: "Nome;CPF;Idade;Ocupação"
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

-- Converte uma linha de texto em um objeto do tipo Filme.
-- Formato esperado: "ID;Título;Gênero;Duração;Sinopse"
parseFilme :: String -> Filme
parseFilme linha = Filme id titulo genero duracao sinopse
    where
        [idStr, titulo, generoStr, duracaoStr, sinopse] = split ';' linha
        id = read idStr
        genero = parseGenero generoStr
        duracao = read duracaoStr

-- Converte uma linha de texto em um objeto do tipo Sessão.
-- Formato esperado: "ID;TítuloFilme;Horário;Dia;TipoSessão;3D;Sala;Assentos"
parseSessao :: [Filme] -> String -> Maybe Sessao
parseSessao filmes linha =
    case (filme, tipoSessao) of
        (Just f, Just ts) -> Just $ Sessao id f horario dia ts is3D sala assentos
        _ -> Nothing
    where
        [idStr, titulo, horarioStr, diaStr, tipoStr, is3DStr, salaStr, assentosStr] = split ';' linha
        id = read idStr
        filme = find ((== read titulo) . getIdFilme) filmes
        horario = parseHorario horarioStr
        dia = parseDia diaStr
        tipoSessao = readMaybe tipoStr
        is3D = read is3DStr
        sala = read salaStr
        assentos = parseAssentos assentosStr

-- Converte uma linha de texto em um objeto do tipo Pedido.
-- Formato esperado: "ID;CPFCliente;IDSessão;Ingressos;Valor"
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

--- Funções auxiliares para mapeamento de strings

-- Converte uma string de gêneros no formato "[Ação,Drama]" para uma lista de strings.
parseGenero :: String -> [String]
parseGenero str = split ',' (filter (`notElem` "[]") str)

-- Converte uma string no formato "HH:MM" para um tipo Horario (tupla de inteiros).
parseHorario :: String -> Horario
parseHorario str =
    let [h, m] = [read x | x <- split ':' str]
    in (h, m)

-- Converte uma string no formato "DD/MM/AAAA" para um tipo Dia (tupla de inteiros).
parseDia :: String -> Dia
parseDia str =
    let [d, m, a] = [read x | x <- split '/' str]
    in (d, m, a)

-- Converte uma string de assentos no formato "[A,1,True;B,2,False]" para uma lista de Assento.
parseAssentos :: String -> [Assento]
parseAssentos str =
    let -- Remove os colchetes ao redor da lista de assentos
        limpar = filter (`notElem` "[]") str
        -- Divide por vírgulas, cada grupo de 3 representará um assento
        assentosStrs = split ',' limpar
    in map (\[fileira, numero, ocupado] -> (head fileira, read numero, read ocupado)) (agrupaAssentos assentosStrs)

-- Agrupa uma lista de strings em grupos de 3 elementos.
-- Exemplo: ["A", "1", "True", "B", "2", "False"] -> [["A", "1", "True"], ["B", "2", "False"]]

-- Agrupa em listas de 3 elementos
agrupaAssentos :: [String] -> [[String]]
agrupaAssentos [] = []
agrupaAssentos (a:b:c:xs) = [a, b, c] : agrupaAssentos xs
agrupaAssentos _ = error "Formato inválido de assento."

-- Converte uma string de ingressos no formato "[Tipo:Assento;Tipo:Assento]" para uma lista de Ingresso.
parseIngressos :: String -> Maybe [Ingresso]
parseIngressos str =
    let cleanStr = filter (`notElem` "[]") str
        ingressosStrs = split ';' cleanStr
    in sequence [ parseIngresso s | s <- ingressosStrs ]

-- Converte uma string no formato "Tipo:Assento" para um objeto Ingresso.
parseIngresso :: String -> Maybe Ingresso
parseIngresso s =
    case split ':' s of
        [tipoStr, assentoStr] -> do
            tipo <- parseTipoIngresso tipoStr
            assento <- parseAssento assentoStr
            Just (tipo, assento)
        _ -> Nothing

-- Converte uma string no formato "Inteira 20.0" ou "Meia" para um TipoIngresso.
parseTipoIngresso :: String -> Maybe TipoIngresso
parseTipoIngresso s =
    case words s of
        ["Inteira", v] -> case readMaybe v of
            Just f -> Just (Inteira f)
            _ -> Nothing
        ["Meia"] -> Just Meia
        _ -> Nothing

-- Converte uma string no formato "A,1,True" para um objeto Assento.
parseAssento :: String -> Maybe Assento
parseAssento s =
    case split ',' s of
        [fStr, nStr, oStr] -> do
            let f = head fStr  -- Assume que fStr é um único caractere
            n <- readMaybe nStr
            o <- readMaybe oStr
            Just (f, n, o)
        _ -> Nothing

--- Funções para formatar objetos em strings

-- Converte um objeto Cliente em uma string no formato "Nome;CPF;Idade;Ocupação".
formatarCliente :: Cliente -> String
formatarCliente (Cliente nome cpf idade ocupacao) =
    nome ++ ";" ++ cpf ++ ";" ++ show idade ++ ";" ++ show ocupacao

-- Converte um objeto Filme em uma string no formato "ID;Título;Gênero;Duração;Sinopse".
formatarFilme :: Filme -> String
formatarFilme (Filme id titulo genero duracao sinopse) =
    show id ++ ";" ++ titulo ++ ";" ++ formatarGenero genero ++ ";" ++ show duracao ++ ";" ++ sinopse

-- Converte uma lista de gêneros em uma string no formato "[Ação,Drama]".
formatarGenero :: Genero -> String
formatarGenero genero = "[" ++ intercalate "," genero ++ "]"

-- Converte um objeto Sessão em uma string no formato "ID;IDFilme;Horário;Dia;TipoSessão;3D;Sala;Assentos".
formatarSessao :: Sessao -> String
formatarSessao (Sessao id (Filme idFilme _ _ _ _) horario dia tipoSessao is3D sala assentos) =
    show id ++ ";" ++ show idFilme ++ ";" ++ formatarHorario horario ++ ";" ++ formatarDia dia ++ ";"
        ++ formatarTipo tipoSessao ++ ";" ++ show is3D ++ ";" ++ show sala ++ ";" ++ formatarAssentos assentos

-- Converte um objeto Assento em uma string no formato "[Fileira,Número,Ocupado]".
formatarAssento :: Assento -> String
formatarAssento (fileira, numero, ocupado) =
    "[" ++ [fileira] ++ "," ++ show numero ++ "," ++ show ocupado ++ "]"

-- Converte uma lista de Assento em uma string no formato "[[A,1,True],[B,2,False]]".
formatarAssentos :: [Assento] -> String
formatarAssentos assentos = "[" ++ intercalate "," [formatarAssento a | a <- assentos] ++ "]"

-- Converte um objeto Horario em uma string no formato "HH:MM".
formatarHorario :: Horario -> String
formatarHorario (h, m) = show h ++ ":" ++ (if m < 10 then "0" ++ show m else show m)

-- Converte um objeto Dia em uma string no formato "DD/MM/AAAA".
formatarDia :: Dia -> String
formatarDia (d, m, a) =
    let formatar n = if n < 10 then "0" ++ show n else show n
    in formatar d ++ "/" ++ formatar m ++ "/" ++ show a

-- Converte um objeto TipoSessao em uma string ("Dublado" ou "Legendado").
formatarTipo :: TipoSessao -> String
formatarTipo Dublado = "Dublado"
formatarTipo Legendado = "Legendado"

-- Converte um objeto Pedido em uma string no formato "ID;CPFCliente;IDSessão;Ingressos;Valor".
formatarPedido :: Pedido -> String
formatarPedido (Ped id cliente sessao ingressos valor) =
    intercalate ";" [show id, getCpf cliente, show (getIdSessao sessao), formatarIngressos ingressos, show valor]

-- Converte uma lista de Ingresso em uma string no formato "[Tipo:Assento;Tipo:Assento]".
formatarIngressos :: [Ingresso] -> String
formatarIngressos ingressos = "[" ++ intercalate ";" [formatarIngresso i | i <- ingressos] ++ "]"

-- Converte um objeto Ingresso em uma string no formato "Tipo:Assento".
formatarIngresso :: Ingresso -> String
formatarIngresso (tipo, assento) = formatarTipoIngresso tipo ++ ":" ++ formatarAssento assento

-- Converte um objeto TipoIngresso em uma string ("Inteira 20.0" ou "Meia").
formatarTipoIngresso :: TipoIngresso -> String
formatarTipoIngresso (Inteira v) = "Inteira " ++ show v
formatarTipoIngresso Meia = "Meia"

-- Converte um objeto Assento em uma string no formato "Fileira,Número,Ocupado".
formatarAssentoPedido :: Assento -> String
formatarAssentoPedido (f, n, o) = [f] ++ "," ++ show n ++ "," ++ show o