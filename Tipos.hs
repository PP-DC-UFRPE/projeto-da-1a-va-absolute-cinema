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

--abaixo seguem as funções para poder pegar o type especifico a partir do filme

pegarTitulo :: Filme -> String
pegarTitulo (Filme t _ _ _) = t

printarTituloEDuracao :: Filme -> IO ()
printarTituloEDuracao f = putStrLn ("Titulo: " ++ pegarTitulo f ++ " Duracao: " ++ show (pegarDuracao f))

pegarGenero :: Filme -> [String]
pegarGenero (Filme _ g _ _) = g

pegarDuracao :: Filme -> Int
pegarDuracao (Filme _ _ d _ ) = d

pegarSinopse :: Filme -> String
pegarSinopse (Filme _ _ _ s) = s

printarFilme :: Filme -> String
printarFilme (Filme t g d s) = "Titulo: " ++ t ++ ", Generos: " ++ intercalate ", " g ++ ", Duracao: " ++ show d ++ ", Sinopse: " ++ s

--abaixo seguem as funções para poder pegar o type especifico a partir do Cliente

pegarNome :: Cliente -> String
pegarNome (Cliente n _ _ _) = n

pegarCPF :: Cliente -> String
pegarCPF (Cliente _ c _ _) = c

pegarIdade :: Cliente -> Int
pegarIdade (Cliente _ _ i _) = i

pegarOcupacao :: Cliente -> Ocupacao
pegarOcupacao (Cliente _ _ _ o) = o

printarCliente :: Cliente -> String
printarCliente (Cliente n c i o) = "Nome: " ++ n ++ ", CPF: " ++ c ++ ", Idade: " ++ show i ++ ", Ocupacao: " ++ show o

--abaixo seguem as funções para poder pegar o type especifico a partir da sessão

pegarTituloDaSessao :: Sessao -> String
pegarTituloDaSessao (Sessao (Filme t _ _ _) _ _ _ _ _) = t

pegarGeneroDaSessao :: Sessao -> [String]
pegarGeneroDaSessao (Sessao (Filme _ g _ _) _ _ _ _ _) = g

pegarDuracaoDaSessao :: Sessao -> Int
pegarDuracaoDaSessao (Sessao (Filme _ _ d _) _ _ _ _ _) =  d

pegarSinopseDaSessao :: Sessao -> String
pegarSinopseDaSessao (Sessao (Filme _ _ _ s) _ _ _ _ _) = s

pegarTipoSessaoDaSessao :: Sessao -> TipoSessao
pegarTipoSessaoDaSessao (Sessao _ _ s _ _ _) = s

--abaixo seguem as funções para poder o type específico a partir do pedido

pegarNomeDoPedido :: Pedido -> String
pegarNomeDoPedido (Ped (Cliente n _ _ _) _ _ _) = n

pegarCPFDoPedido :: Pedido -> String
pegarCPFDoPedido (Ped (Cliente _ c _ _) _ _ _) = c

pegarIdadeDoPedido :: Pedido -> Int
pegarIdadeDoPedido (Ped (Cliente _ _ i _) _ _ _) = i

pegarOcupacaoDoPedido :: Pedido -> Ocupacao
pegarOcupacaoDoPedido (Ped (Cliente _ _ _ o) _ _ _) = o 

--abaixo seguem as funções para poder mexer com o Sistema

printarFilmes :: Sistema -> IO () 
printarFilmes (_, f, _, _) = do
    mapM_ printarTituloEDuracao f

printarSessoesPorFilme :: [Sessao] -> Filme -> IO ()
printarSessoesPorFilme sessoes filme = 
    mapM_ (\(Sessao _ (h, m) t _ sala _) -> putStrLn ("  Sessao: " ++ show h ++ ":" ++ show m ++ ", " ++ show t ++ ", Sala " ++ show sala)) (filter (\(Sessao f _ _ _ _ _) -> f == filme) sessoes)

printarFilmesESessoes :: Sistema -> IO ()
printarFilmesESessoes (_, filmes, sessoes, _) = do
    mapM_ (\filme -> do
        printarTituloEDuracao filme
        printarSessoesPorFilme sessoes filme) filmes
