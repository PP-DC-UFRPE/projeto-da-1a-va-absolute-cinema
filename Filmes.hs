module Filmes where
import Tipos
import Dados ()
import Data.IORef
import Data.Traversable (for)
import System.IO

-- Exibe os filmes disponíveis e suas sessões
exibicao :: IORef Sistema -> IO ()
exibicao sistemaRef = do
    sistema <- readIORef sistemaRef
    putStrLn "Esses são os filmes disponíveis hoje:"
    printarFilmesESessoes sistema
    putStrLn ""

-- Formata o gênero do filme para exibição
formatarGenero :: Genero -> String
formatarGenero genero = '(' : unwords (map (++ ", ") (init genero)) ++ last genero ++ ")"

-- Formata um filme para exibição
printarFilme :: Filme -> IO ()
printarFilme (Filme id titulo genero duracao sinopse) = do
    putStrLn $ "ID: " ++ show id ++ " - " ++ titulo
    putStrLn $ "Gênero: " ++ formatarGenero genero
    putStrLn $ "Duração: " ++ show duracao ++ " minutos"
    putStrLn $ "Sinopse: " ++ sinopse
    putStrLn "________________________________________________________"

-- Exibe todos os filmes
exibirFilmes :: IORef Sistema -> IO ()
exibirFilmes sistemaRef = do
    filmes <- pegarFilmes sistemaRef
    putStrLn "----- Exibir Filmes -----"
    mapM_ printarFilme filmes
    putStrLn ""

gerarIdFilme :: [Filme] -> Id
gerarIdFilme filmes = 
    if null filmes 
        then 1 
        else maximum (map getIdFilme filmes) + 1

adicionarFilme :: Filme -> IORef Sistema -> IO ()
adicionarFilme filme sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    writeIORef sistemaRef (usuarios, filme:filmes, sessoes, pedidos)

menuAdicionarFilme :: IORef Sistema -> IO ()
menuAdicionarFilme sistemaRef = do
    (_, filmes, _, _) <- readIORef sistemaRef
    putStrLn "----- Adicionar Filme -----"
    putStrLn "Digite o título do filme"
    putStr "Título: "
    hFlush stdout
    titulo <- getLine
    putStrLn "Digite o gênero do filme (separado por vírgulas)"
    putStr "Gênero: "
    hFlush stdout
    genero <- fmap words getLine
    putStrLn "Digite a duração do filme (em minutos)"
    putStr "Duração: "
    hFlush stdout
    duracao <- fmap read getLine
    putStrLn "Digite a sinopse do filme"
    putStr "Sinopse: "
    hFlush stdout
    sinopse <- getLine
    
    let filme = Filme (gerarIdFilme filmes) titulo genero duracao sinopse
    adicionarFilme filme sistemaRef
    putStrLn "\nFilme adicionado com sucesso!"
    putStrLn ""