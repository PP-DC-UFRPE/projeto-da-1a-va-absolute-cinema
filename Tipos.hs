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
data Filme = Filme Titulo Genero Duracao Sinopse deriving Show

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
