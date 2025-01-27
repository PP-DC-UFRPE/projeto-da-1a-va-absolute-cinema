module Filmes where
import Tipos
import Dados
import Data.IORef
import Data.Traversable (for)
import System.IO

-- Exibe os filmes disponíveis e suas sessões
exibicao :: IORef Sistema -> IO ()
exibicao sistemaRef = do
    sistema <- readIORef sistemaRef
    putStrLn "Esses são os filmes disponíveis:"
    printarFilmesESessoes sistema
    putStrLn ""

-- Formata o gênero do filme para exibição
formatarGenero :: Genero -> String
formatarGenero genero = '[' : unwords (map (++ ", ") (init genero)) ++ last genero ++ "]"

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

buscarFilme :: Id -> [Filme] -> Maybe Filme
buscarFilme _ [] = Nothing
buscarFilme id (f:fs) = if getIdFilme f == id then Just f else buscarFilme id fs

adicionarFilme :: Filme -> IORef Sistema -> IO ()
adicionarFilme filme sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    writeIORef sistemaRef (usuarios, filme:filmes, sessoes, pedidos)

removerFilme :: Id -> IORef Sistema -> IO ()
removerFilme id sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let filmesAtualizados = filter (\f-> getIdFilme f /= id) filmes
    if length filmesAtualizados == length filmes
        then putStrLn "Filme não encontrado"
        else do 
            putStrLn "Filme removido com sucesso"
            writeIORef sistemaRef (usuarios, filmesAtualizados, sessoes, pedidos)

editarFilme :: Filme -> IORef Sistema -> IO ()
editarFilme filme sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let filmesAtualizados = map (\f -> if getIdFilme f == getIdFilme filme then filme else f) filmes
    writeIORef sistemaRef (usuarios, filmesAtualizados, sessoes, pedidos)
    putStrLn "\nFilme editado com sucesso"

--Menus de gerenciamento de filmes

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
    duracaoIpt <- getLine
    let duracao = read duracaoIpt :: Int
    putStrLn "Digite a sinopse do filme"
    putStr "Sinopse: "
    hFlush stdout
    sinopse <- getLine
    let filme = Filme (gerarIdFilme filmes) titulo genero duracao sinopse
    adicionarFilme filme sistemaRef
    putStrLn "\nFilme adicionado com sucesso!"
    putStrLn ""

menuRemoverFilme :: IORef Sistema -> IO ()
menuRemoverFilme sistemaRef = do
    putStrLn "----- Remover Filme -----"
    putStrLn "Digite o ID do filme que deseja remover"
    putStr "ID: "
    hFlush stdout
    input <- getLine
    let id = read input :: Id
    removerFilme id sistemaRef

menuEditarFilme :: IORef Sistema -> IO ()
menuEditarFilme sistemaRef = do
    (_, filmes, _, _) <- readIORef sistemaRef
    putStrLn "----- Editar Filme -----"
    putStrLn "Digite o ID do filme que deseja editar"
    putStr "ID: "
    hFlush stdout
    input <- getLine
    let id = read input :: Id
    let filme = buscarFilme id filmes
    case filme of
        Nothing -> putStrLn "\nFilme não encontrado"
        Just f -> do
            putStrLn "\nDigite o novo título do filme"
            putStr "Título: "
            hFlush stdout
            titulo <- getLine
            putStrLn "Digite o novo gênero do filme (separado por vírgulas)"
            putStr "Gênero: "
            hFlush stdout
            genero <- fmap words getLine
            putStrLn "Digite a nova duração do filme (em minutos)"
            putStr "Duração: "
            hFlush stdout
            duracaoIpt <- getLine
            let duracao = read duracaoIpt :: Int
            putStrLn "Digite a nova sinopse do filme"
            putStr "Sinopse: "
            hFlush stdout
            sinopse <- getLine
            let filmeAtualizado = Filme id titulo genero duracao sinopse
            editarFilme filmeAtualizado sistemaRef