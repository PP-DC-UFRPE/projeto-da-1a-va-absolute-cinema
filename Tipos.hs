module Tipos where
import Data.List
import Data.IORef
import System.IO

-- Senha do administrador
senhaAdmin :: String
senhaAdmin = "admin123"

type Id = Int

type Nome = String
type Cpf = String
type Idade = Int
data Ocupacao = Estudante | Professor | Outras deriving Show
data Cliente = Cliente {getNome :: Nome, getCpf :: Cpf, getIdade :: Idade, getOcupacao :: Ocupacao} deriving Show

instance Eq Cliente where
    (Cliente _ cpf1 _ _) == (Cliente _ cpf2 _ _) = cpf1 == cpf2

type Titulo = String
type Genero = [String]
type Duracao = Int -- Minutos
type Sinopse = String
data Filme = Filme {getIdFilme :: Id, getTitulo :: Titulo, getGenero :: Genero, getDuracao :: Duracao, getSinopse :: Sinopse} deriving Show

instance Eq Filme where
    (Filme id1 t1 _ _ _) == (Filme id2 t2 _ _ _) = id1 == id2 && t1 == t2

type Horario = (Int, Int)-- Hora/Minuto
type Dia = (Int, Int, Int) -- Dia/Mes/Ano
data TipoSessao = Dublado | Legendado deriving (Show, Read)
type Is3D = Bool
type Sala = Int
type Assento = (Char, Int, Bool) -- Letra da Fileira/Numero Assento/Ocupado
data Sessao = Sessao {getIdSessao :: Id, getFilme :: Filme, getHorario :: Horario, getDia :: Dia, getTipo :: TipoSessao, getIs3D :: Is3D, getSala :: Sala, getAssentos :: [Assento]} deriving Show

instance Eq Sessao where
    (Sessao id1 _ _ _ _ _ _ _) == (Sessao id2 _ _ _ _ _ _ _) = id1 == id2

valorInteira :: Float
valorInteira = 20.0

data TipoIngresso = Inteira | Meia deriving Show
type Ingresso = (TipoIngresso, Assento)
type Valor = Float
data Pedido = Ped {getIdPedido :: Id, getCliente :: Cliente, getSessao :: Sessao, getIngressos :: [Ingresso], getValor :: Valor} deriving Show

instance Eq Pedido where
    (Ped id1 _ _ _ _) == (Ped id2 _ _ _ _) = id1 == id2

type Sistema = ([Cliente],[Filme],[Sessao],[Pedido])

-- Funções auxiliares para manipular dados e exibir informações

pegarClientes :: IORef Sistema -> IO [Cliente]
pegarClientes sistemaRef = do
    (clientes, _, _, _) <- readIORef sistemaRef
    return clientes

pegarFilmes :: IORef Sistema -> IO [Filme]
pegarFilmes sistemaRef = do
    (_, filmes, _, _) <- readIORef sistemaRef
    return filmes

pegarSessoes :: IORef Sistema -> IO [Sessao]
pegarSessoes sistemaRef = do
    (_, _, sessoes, _) <- readIORef sistemaRef
    return sessoes

pegarPedidos :: IORef Sistema -> IO [Pedido]
pegarPedidos sistemaRef = do
    (_, _, _, pedidos) <- readIORef sistemaRef
    return pedidos

-- Calcula o valor total dos ingressos
calcularValor :: [Ingresso] -> Float
calcularValor = sum . map (\(tipo, _) -> case tipo of
    Inteira -> valorInteira -- se for inteiro, retorna o valor
    Meia -> valorInteira/2) -- se for meia, retorna metade do valor

-- Pega o título
pegarTitulo :: Filme -> String
pegarTitulo (Filme _ t _ _ _) = t

-- Pega a duração
pegarDuracao :: Filme -> Int
pegarDuracao (Filme _ _ _ d _) = d

-- Exibe gênero do filme como uma string formatada
printarGenero :: Genero -> String
printarGenero genero = '(' : unwords (map (++ ", ") (init genero)) ++ last genero ++ ")"

-- Impressão de filmes e sessões
printarFilmeInfo :: Filme -> IO ()
printarFilmeInfo f = do putStrLn $ "Titulo: " ++ pegarTitulo f ++ " " ++ printarGenero (getGenero f) ++ " - Duracao: " ++ show (pegarDuracao f) ++ " min"
                        putStrLn $ "Sinopse: " ++ getSinopse f

-- Exibe todos os filmes e suas respectivas sessões
printarFilmesESessoes :: Sistema -> IO ()
printarFilmesESessoes (_, filmes, sessoes, _) = do
    mapM_ (\filme -> do
        printarFilmeInfo filme
        printarSessoesPorFilme sessoes filme) filmes

-- Exibe as sessões de um filme específico
printarSessoesPorFilme :: [Sessao] -> Filme -> IO ()
printarSessoesPorFilme sessoes filme = do
    let sessoesDoFilme = filter (\(Sessao _ f _ _ _ _ _ _) -> f == filme) sessoes
    putStrLn "\nSessões disponíveis: "
    mapM_ (\(i, Sessao _ _ (h, m) (d, mo, a) t _ sala _) ->
        putStrLn $ show i ++ ") Sessao: " ++ show h ++ ":" ++ show m ++
                   " - " ++ show d ++ "/" ++ show mo ++ "/" ++ show a ++
                   " - " ++ show t ++ " - Sala " ++ show sala) (zip [0..] sessoesDoFilme)
    putStrLn "____________________________________________"

pegarSessoesPorFilme :: [Sessao] -> Filme -> [Sessao]
pegarSessoesPorFilme sessoes filme = filter (\(Sessao _ f _ _ _ _ _ _) -> f == filme) sessoes

-- Formata um assento para exibição com "Disponível" ou "Ocupado"
formatarExibirAssento :: Assento -> String
formatarExibirAssento (letra, numero, ocupado) =
    let status = if ocupado then "Ocupado" else "Disponível"
    in "(" ++ [letra] ++ show numero ++ ", " ++ status ++ ")"

-- Obs: Função feita usando IA, o codigo para gerar uma matriz estava com erros, o que não estava ficando da forma que deveria. Por isso solicitamos o uso da IA para ajudar a criar uma matriz.
-- Exibe os assentos disponíveis na sessão selecionada
printarAssentosPorNumeroSessao :: Int -> [Sessao] -> IO ()
printarAssentosPorNumeroSessao salaNum sessoes = do
    let sessao = filter (\(Sessao _ _ _ _ _ _ n _) -> n == salaNum) sessoes
    case sessao of
        [] -> putStrLn "Sala não encontrada!"
        (Sessao _ _ _ (d, mo, a) _ _ _ assentos : _) -> do
            putStrLn $ "Assentos disponíveis na sala (Dia: " ++ show d ++ "/" ++ show mo ++ "/" ++ show a ++ "):"
            let sortedAssentos = sortBy (\(a1, n1, _) (a2, n2, _) -> 
                                        case compare a1 a2 of
                                            EQ -> compare n1 n2
                                            other -> other) assentos
                groupedAssentos = groupBy (\(a1, _, _) (a2, _, _) -> a1 == a2) sortedAssentos
                allSeatNumbers = [ n | (_, n, _) <- assentos ]
                maxSeat = if null allSeatNumbers then 0 else maximum allSeatNumbers
                header = "    " ++ unwords (map (\n -> if n < 10 then " " ++ show n else show n) [1..maxSeat])
            putStrLn header
            mapM_ (\group -> do
                let (letra, _, _) = head group
                    seatsInRow = [ (n, ocup) | (_, n, ocup) <- group ]
                    statuses = [ case lookup num seatsInRow of
                                    Just True  -> "X"
                                    Just False -> " "
                                    Nothing    -> " "
                                | num <- [1..maxSeat] ]
                    rowStr = letra : "  | " ++ unwords (map (\s -> "[" ++ s ++ "]") statuses)
                putStrLn rowStr
                ) groupedAssentos

-- Printa assentos das sessões
printarAssentosDasSessoes :: [Sessao] -> IO ()
printarAssentosDasSessoes sessoes = mapM_ printarAssentosDaSessao sessoes

-- Printa assentos da sessão específica
printarAssentosDaSessao :: Sessao -> IO ()
printarAssentosDaSessao sessao = mapM_ print (pegarAssentosDaSessao sessao)

-- Pega os assentos da sessão
pegarAssentosDaSessao :: Sessao -> [Assento]
pegarAssentosDaSessao (Sessao _ _ _ _ _ _ _ assentos) = assentos
