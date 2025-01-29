module Tipos where
    
import Data.List
import Data.IORef
import System.IO()

-- Senha do administrador
senhaAdmin :: String
senhaAdmin = "admin123"

type Nome = String
type Cpf = String
type Idade = Int
data Ocupacao = Estudante | Professor | Outras deriving Show
data Cliente = Cliente {getNome :: Nome, getCpf :: Cpf, getIdade :: Idade, getOcupacao :: Ocupacao} deriving Show

instance Eq Cliente where
    (Cliente _ cpf1 _ _) == (Cliente _ cpf2 _ _) = cpf1 == cpf2

type Id = Int
type Titulo = String
type Genero = [String]
type Duracao = Int -- Minutos
type Sinopse = String
data Filme = Filme {getIdFilme :: Id, getTitulo :: Titulo, getGenero :: Genero, getDuracao :: Duracao, getSinopse :: Sinopse} deriving Show

instance Eq Filme where
    (Filme id1 t1 _ _ _) == (Filme id2 t2 _ _ _) = id1 == id2 && t1 == t2

type Horario = (Int, Int) -- Hora/Minuto
type Dia = (Int, Int, Int) -- Dia/Mes/Ano
data TipoSessao = Dublado | Legendado deriving (Show, Read)
type Is3D = Bool
type Sala = Int
type Assento = (Char, Int, Bool) -- Letra da Fileira/Numero Assento/Ocupado
data Sessao = Sessao {getIdSessao :: Id, getFilme :: Filme, getHorario :: Horario, getDia :: Dia, getTipo :: TipoSessao, getIs3D :: Is3D, getSala :: Sala, getAssentos :: [Assento]} deriving Show

instance Eq Sessao where
    (Sessao id1 _ _ _ _ _ _ _) == (Sessao id2 _ _ _ _ _ _ _) = id1 == id2

data TipoIngresso = Inteira Float | Meia deriving Show
type Ingresso = (TipoIngresso, Assento)
type Valor = Float
data Pedido = Ped {getIdPedido :: Id, getCliente :: Cliente, getSessao :: Sessao, getIngressos :: [Ingresso], getValor :: Valor} deriving Show

instance Eq Pedido where
    (Ped id1 _ _ _ _) == (Ped id2 _ _ _ _) = id1 == id2

type Sistema = ([Cliente],[Filme],[Sessao],[Pedido])

-- Funções auxiliares para manipular dados e exibir informações

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
    Inteira v -> v -- se for inteiro, retorna o valor
    Meia      -> 10.0) -- se for meia, somente 10

-- Pega o título
pegarTitulo :: Filme -> String
pegarTitulo (Filme _ t _ _ _) = t

-- Pega a duração
pegarDuracao :: Filme -> Int
pegarDuracao (Filme _ _ _ d _) = d

-- Impressão de filmes e sessões
printarTituloEDuracao :: Filme -> IO ()
printarTituloEDuracao f = putStrLn ("Titulo: " ++ pegarTitulo f ++ " Duracao: " ++ show (pegarDuracao f))

-- Exibe todos os filmes e suas respectivas sessões
printarFilmesESessoes :: Sistema -> IO ()
printarFilmesESessoes (_, filmes, sessoes, _) = do
    mapM_ (\filme -> do
        printarTituloEDuracao filme
        printarSessoesPorFilme sessoes filme) filmes

-- Exibe as sessões de um filme específico
printarSessoesPorFilme :: [Sessao] -> Filme -> IO ()
printarSessoesPorFilme sessoes filme =
    mapM_ (\(Sessao _ _ (h, m) (d, mo, a) t _ sala _) ->
        putStrLn $ "  Sessao: " ++ show h ++ ":" ++ show m ++
                   ", Dia: " ++ show d ++ "/" ++ show mo ++ "/" ++ show a ++
                   ", " ++ show t ++ ", Sala " ++ show sala) 
        (filter (\(Sessao _ f _ _ _ _ _ _) -> f == filme) sessoes)


-- Formata um assento para exibição com "Disponível" ou "Ocupado"
formatarAssento :: Assento -> String
formatarAssento (letra, numero, ocupado) =
    let status = if ocupado then "Ocupado" else "Disponível"
    in "(" ++ [letra] ++ show numero ++ ", " ++ status ++ ")"

-- Exibe os assentos disponíveis na sessão selecionada
printarAssentosPorNumeroSessao :: Int -> [Sessao] -> IO ()
printarAssentosPorNumeroSessao salaNum sessoes = do
    let sessao = filter (\(Sessao _ _ _ _ _ _ n _) -> n == salaNum) sessoes
    case sessao of
        [] -> putStrLn "Sala não encontrada!"
        (Sessao _ _ _ (d, mo, a) _ _ _ assentos : _) -> do
            putStrLn $ "Assentos disponíveis na sala (Dia: " ++ show d ++ "/" ++ show mo ++ "/" ++ show a ++ "):"
            mapM_ (putStrLn . formatarAssento) assentos

-- Printa assentos das sessões
printarAssentosDasSessoes :: [Sessao] -> IO ()
printarAssentosDasSessoes sessoes = mapM_ printarAssentosDaSessao sessoes

-- Printa assentos da sessão específica
printarAssentosDaSessao :: Sessao -> IO ()
printarAssentosDaSessao sessao = mapM_ print (pegarAssentosDaSessao sessao)

-- Pega os assentos da sessão
pegarAssentosDaSessao :: Sessao -> [Assento]
pegarAssentosDaSessao (Sessao _ _ _ _ _ _ _ assentos) = assentos

-- Atualização de assentos
atualizarAssento :: Char -> Int -> [Sessao] -> [Sessao]
atualizarAssento letra numAssento sessoes =
    map (\(Sessao id filme horario dia tipo3D isSala sala assentos) -> 
            Sessao id filme horario dia tipo3D isSala sala (map (atualizar letra numAssento) assentos)
        ) sessoes
  where
    atualizar l n (lAssento, nAssento, ocupado)
        | l == lAssento && n == nAssento = (lAssento, nAssento, True)
        | otherwise                      = (lAssento, nAssento, ocupado)
