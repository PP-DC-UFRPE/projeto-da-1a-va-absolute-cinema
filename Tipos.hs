module Tipos where
import Data.List

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
data TipoSessao = Dublado | Legendado deriving Show
type Is3D = Bool
type Sala = Int
type Assento = (Char, Int, Bool) -- Letra da Fileira/Numero Assento/Ocupado
data Sessao = Sessao Filme Horario TipoSessao Is3D Sala [Assento] deriving Show

data TipoIngresso = Inteira Float | Meia deriving Show
type Ingresso = (TipoIngresso, Assento)
type Valor = Float
data Pedido = Ped Cliente Sessao [Ingresso] Valor deriving Show

type Sistema = ([Cliente],[Filme],[Sessao],[Pedido])

-- Funções do sistema
pegarTitulo :: Filme -> String
pegarTitulo (Filme t _ _ _) = t

printarTituloEDuracao :: Filme -> IO ()
printarTituloEDuracao f = putStrLn ("Titulo: " ++ pegarTitulo f ++ " Duracao: " ++ show (pegarDuracao f))

pegarDuracao :: Filme -> Int
pegarDuracao (Filme _ _ d _) = d

printarFilmesESessoes :: Sistema -> IO ()
printarFilmesESessoes (_, filmes, sessoes, _) = do
    mapM_ (\filme -> do
        printarTituloEDuracao filme
        printarSessoesPorFilme sessoes filme) filmes

printarSessoesPorFilme :: [Sessao] -> Filme -> IO ()
printarSessoesPorFilme sessoes filme =
    mapM_ (\(Sessao _ (h, m) t _ sala _) -> putStrLn ("  Sessao: " ++ show h ++ ":" ++ show m ++ ", " ++ show t ++ ", Sala " ++ show sala)) (filter (\(Sessao f _ _ _ _ _) -> f == filme) sessoes)

printarAssentosPorNumeroSessao :: Int -> [Sessao] -> IO ()
printarAssentosPorNumeroSessao numeroSala sessoes = do
    let sessoesFiltradas = filter (\(Sessao _ _ _ _ numero _) -> numero == numeroSala) sessoes
    printarAssentosDasSessoes sessoesFiltradas


printarAssentosDasSessoes :: [Sessao] -> IO ()
printarAssentosDasSessoes sessoes = mapM_ printarAssentosDaSessao sessoes

printarAssentosDaSessao :: Sessao -> IO ()
printarAssentosDaSessao sessao = mapM_ print (pegarAssentosDaSessao sessao)

pegarAssentosDaSessao :: Sessao -> [Assento]
pegarAssentosDaSessao (Sessao _ _ _ _ _ assentos) = assentos

atualizarAssento :: Char -> Int -> [Sessao] -> [Sessao]
atualizarAssento letra numAssento sessoes =
    map (\(Sessao filme horario tipo3D isSala sala assentos) ->
            Sessao filme horario tipo3D isSala sala (map (atualizar letra numAssento) assentos)
        ) sessoes
  where
    atualizar l n (lAssento, nAssento, ocupado)
        | l == lAssento && n == nAssento = (lAssento, nAssento, True)
        | otherwise                      = (lAssento, nAssento, ocupado)
