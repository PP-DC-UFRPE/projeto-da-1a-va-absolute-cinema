module Tipos where
import Data.List
import Data.IORef
import System.IO 

type Nome = String
type Cpf = String
type Idade = Int
data Ocupacao = Estudante | Professor | Outras deriving Show
data Cliente = Cliente Nome Cpf Idade Ocupacao deriving Show

type Titulo = String
type Genero = [String]
type Duracao = Int -- Minutos
type Sinopse = String
data Filme = Filme Titulo Genero Duracao Sinopse deriving (Show, Eq)

type Horario = (Int, Int) -- Hora/Minuto
type Dia = (Int, Int, Int) -- Dia/Mes/Ano
data TipoSessao = Dublado | Legendado deriving (Show, Read)
type Is3D = Bool
type Sala = Int
type Assento = (Char, Int, Bool) -- Letra da Fileira/Numero Assento/Ocupado
data Sessao = Sessao Filme Horario TipoSessao Is3D Sala [Assento] deriving Show

data TipoIngresso = Inteira Float | Meia deriving Show
type Ingresso = (TipoIngresso, Assento)
type Valor = Float
data Pedido = Ped Cliente Sessao [Ingresso] Valor deriving Show

type Sistema = ([Cliente],[Filme],[Sessao],[Pedido])

-- Funções auxiliares para manipular dados e exibir informações
-- Calcula o valor total dos ingressos
calcularValor :: [Ingresso] -> Float
calcularValor = sum . map (\(tipo, _) -> case tipo of
    Inteira v -> v -- se for inteiro, retorna o valor
    Meia      -> 10.0) -- se for meia, somente 10

-- Pega o título
pegarTitulo :: Filme -> String
pegarTitulo (Filme t _ _ _) = t

-- Pega a duração
pegarDuracao :: Filme -> Int
pegarDuracao (Filme _ _ d _) = d

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
    mapM_ (\(Sessao _ (h, m) t _ sala _) -> putStrLn ("  Sessao: " ++ show h ++ ":" ++ show m ++ ", " ++ show t ++ ", Sala " ++ show sala)) (filter (\(Sessao f _ _ _ _ _) -> f == filme) sessoes)

-- Formata um assento para exibição com "Disponível" ou "Ocupado"
formatarAssento :: Assento -> String
formatarAssento (letra, numero, ocupado) =
    let status = if ocupado then "Ocupado" else "Disponível"
    in "(" ++ [letra] ++ show numero ++ ", " ++ status ++ ")"

-- Exibe os assentos disponíveis na sessão selecionada
printarAssentosPorNumeroSessao :: Int -> [Sessao] -> IO ()
printarAssentosPorNumeroSessao salaNum sessoes = do
    let sessao = filter (\(Sessao _ _ _ _ n _) -> n == salaNum) sessoes
    case sessao of
        [] -> putStrLn "Sala não encontrada!"
        (Sessao _ _ _ _ _ assentos : _) -> do
            putStrLn "Assentos disponíveis na sala:"
            mapM_ (putStrLn . formatarAssento) assentos

-- Printa assentos das sessões
printarAssentosDasSessoes :: [Sessao] -> IO ()
printarAssentosDasSessoes sessoes = mapM_ printarAssentosDaSessao sessoes

-- Printa assentos da sessão específica
printarAssentosDaSessao :: Sessao -> IO ()
printarAssentosDaSessao sessao = mapM_ print (pegarAssentosDaSessao sessao)

-- Pega os assentos da sessão
pegarAssentosDaSessao :: Sessao -> [Assento]
pegarAssentosDaSessao (Sessao _ _ _ _ _ assentos) = assentos

-- Atualização de assentos
atualizarAssento :: Char -> Int -> [Sessao] -> [Sessao]
atualizarAssento letra numAssento sessoes =
    map (\(Sessao filme horario tipo3D isSala sala assentos) -> 
            Sessao filme horario tipo3D isSala sala (map (atualizar letra numAssento) assentos)
        ) sessoes
  where
    atualizar l n (lAssento, nAssento, ocupado)
        | l == lAssento && n == nAssento = (lAssento, nAssento, True)
        | otherwise                      = (lAssento, nAssento, ocupado)

-- Funções relacionadas à compra de ingressos
--
-- Printa todos os ingressos
visualizarIngressos :: IORef Sistema -> IO ()
visualizarIngressos sistemaRef = do
    sistema <- readIORef sistemaRef
    let pedidos = pegarPedidos sistema
    if null pedidos
        then putStrLn "Nenhum ingresso foi comprado ainda.\n"
        else do
            putStrLn "Ingressos comprados:\n"
            mapM_ exibirPedido pedidos
            putStrLn ""

-- Exibe os ingressos
exibirPedido :: Pedido -> IO ()
exibirPedido (Ped (Cliente nome cpf _ _) (Sessao (Filme titulo _ _ _) (h, m) tipo _ sala _) ingressos valor) = do
    putStrLn $ "Cliente: " ++ nome ++ " (CPF: " ++ cpf ++ ")"
    putStrLn $ "Filme: " ++ titulo
    putStrLn $ "Horário: " ++ show h ++ ":" ++ show m
    putStrLn $ "Sala: " ++ show sala ++ " | Sessão: " ++ show tipo
    putStrLn $ "Ingressos: " ++ show (length ingressos) ++ " - Valor total: R$ " ++ show valor
    mapM_ exibirIngresso ingressos
    putStrLn "--------------------------------"

exibirIngresso :: Ingresso -> IO ()
exibirIngresso (tipo, (letra, num, _)) = do
    putStrLn $ "  Assento: " ++ [letra] ++ show num ++ " | " ++ tipoToString tipo

tipoToString :: TipoIngresso -> String
tipoToString (Inteira v) = "Inteira - R$ " ++ show v
tipoToString Meia        = "Meia - R$ 10.00"

pegarPedidos :: Sistema -> [Pedido]
pegarPedidos (_, _, _, pedidos) = pedidos
