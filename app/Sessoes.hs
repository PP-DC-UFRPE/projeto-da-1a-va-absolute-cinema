{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Sessoes where
import Tipos
import Dados
import Data.IORef
import System.IO

printarSessao :: Sessao -> IO ()
printarSessao (Sessao id filme horario dia tipo is3d sala assentos) = do
    putStrLn $ "ID: " ++ show id ++ " - " ++ pegarTitulo filme
    putStrLn $ formatarTipo tipo ++ " - " ++ (if is3d then "3D" else "Normal") ++ " - Sala " ++ show sala
    putStrLn $ "Horario: " ++ formatarHorario horario ++ " | Data: " ++ formatarDia dia
    putStrLn "________________________________________________________"
    
exibirSessoes :: IORef Sistema -> IO ()
exibirSessoes sistemaRef = do
    sessoes <- pegarSessoes sistemaRef
    putStrLn "----- Exibir Sessões -----"
    mapM_ printarSessao sessoes
    putStrLn ""

buscarSessao :: Id -> [Sessao] -> Maybe Sessao
buscarSessao _ [] = Nothing
buscarSessao id (s:ss) = if getIdSessao s == id then Just s else buscarSessao id ss

adicionarSessao :: Sessao -> IORef Sistema -> IO ()
adicionarSessao sessao sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    writeIORef sistemaRef (usuarios, filmes, sessao:sessoes, pedidos)

removerSessao :: Id -> IORef Sistema -> IO ()
removerSessao id sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let sessoesAtualizadas = filter (\s -> getIdSessao s /= id) sessoes
    if length sessoesAtualizadas == length sessoes
        then putStrLn "\nSessão não encontrada"
        else do 
            putStrLn "\nSessão removida com sucesso"
            writeIORef sistemaRef (usuarios, filmes, sessoesAtualizadas, pedidos)

editarSessao :: Sessao -> IORef Sistema -> IO ()
editarSessao sessao sistemaRef = do
    (usuarios, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let sessoesAtualizadas = map (\s -> if getIdSessao s == getIdSessao sessao then sessao else s) sessoes
    writeIORef sistemaRef (usuarios, filmes, sessoesAtualizadas, pedidos)
    putStrLn "\nSessao editada com sucesso"

gerarIdSessao :: [Sessao] -> Id
gerarIdSessao sessoes =
    if null sessoes
        then 1
        else maximum (map getIdSessao sessoes) + 1

gerarAssentos :: [Assento]
gerarAssentos = do
    fileiras <- ['A'..'H']
    assentos <- [1..10]
    return (fileiras, assentos, False)

stringToHorario :: String -> Horario
stringToHorario str = (read (take 2 str), read (drop 3 str))

stringToDia :: String -> Dia
stringToDia str = (read (take 2 str), read (take 2 (drop 3 str)), read (drop 6 str))

menuAdicionarSessao :: IORef Sistema -> IO ()
menuAdicionarSessao sistemaRef = do
    (_, _, sessoes, _) <- readIORef sistemaRef
    putStrLn "\n----- Adicionar Sessão -----"
    filmes <- pegarFilmes sistemaRef
    putStrLn "Filmes disponíveis:"
    mapM_ (\(i, filme) -> putStrLn $ show i ++ ") " ++ pegarTitulo filme) (zip [0..] filmes)
    putStr "Escolha um filme: "
    hFlush stdout
    inputFilme <- getLine
    if notElem inputFilme (map show [0..length filmes - 1])
        then do
            putStrLn "Filme não encontrado!"
            menuAdicionarSessao sistemaRef
    else do
        let filme = filmes !! read inputFilme
        putStrLn $ "Selecionado: " ++ pegarTitulo filme
        putStr "\nHorário (HH:MM): "
        hFlush stdout
        inputHorario <- getLine
        let horario = stringToHorario inputHorario
        putStr "Data (DD/MM/AAAA): "
        hFlush stdout
        inputDia <- getLine
        let dia = stringToDia inputDia
        putStrLn "\nTipo de sessão:"
        putStrLn "1) Dublado"
        putStrLn "2) Legendado"
        putStr "Escolha uma opção: "
        hFlush stdout
        inputTipo <- getLine
        let tipo = case inputTipo of
                "1" -> Dublado
                "2" -> Legendado
                _ -> Dublado
        putStrLn "\n3D?"
        putStrLn "1) Sim"
        putStrLn "2) Não"
        putStr "Escolha uma opção: "
        hFlush stdout
        input3D <- getLine
        let is3d = input3D == "1"
        putStr "\nSala: "
        hFlush stdout
        inputSala <- getLine
        let sala = read inputSala
        let assentos = gerarAssentos
            sessao = Sessao (gerarIdSessao sessoes) filme horario dia tipo is3d sala assentos
        adicionarSessao sessao sistemaRef
        putStrLn "\nSessao adicionada com sucesso!"
        putStrLn ""

menuRemoverSessao :: IORef Sistema -> IO ()
menuRemoverSessao sistemaRef = do
    putStrLn "----- Remover Sessao -----"
    putStrLn "Digite o ID da Sessao que deseja remover"
    putStr "ID: "
    hFlush stdout
    input <- getLine
    let id = read input :: Id
    removerSessao id sistemaRef

menuEditarSessao :: IORef Sistema -> IO ()
menuEditarSessao sistemaRef = do
    (_, _, sessoes, _) <- readIORef sistemaRef
    putStrLn "----- Editar Sessao -----"
    putStrLn "Digite o ID da Sessao que deseja editar"
    putStr "ID: "
    hFlush stdout
    input <- getLine
    let id = read input :: Id
    let sessao = buscarSessao id sessoes
    case sessao of
        Nothing -> putStrLn "\nSessao não encontrada"
        Just s -> do
            putStrLn "\nSessao selecionada: "
            printarSessao s
            putStrLn "\nDigite o novo horário da sessão (HH:MM)"
            putStr "Horário: "
            hFlush stdout
            inputHorario <- getLine
            let horario = stringToHorario inputHorario
            putStrLn "\nDigite a nova data da sessão (DD/MM/AAAA)"
            putStr "Data: "
            hFlush stdout
            inputDia <- getLine
            let dia = stringToDia inputDia
            putStrLn "\nTipo de sessão:"
            putStrLn "1) Dublado"
            putStrLn "2) Legendado"
            putStr "Escolha uma opção: "
            hFlush stdout
            inputTipo <- getLine
            let tipo = case inputTipo of
                    "1" -> Dublado
                    "2" -> Legendado
                    _ -> Dublado
            putStrLn "\n3D?"
            putStrLn "1) Sim"
            putStrLn "2) Não"
            putStr "Escolha uma opção: "
            hFlush stdout
            input3D <- getLine
            let is3d = input3D == "1"
            putStr "\nSala: "
            hFlush stdout
            inputSala <- getLine
            let sala = read inputSala :: Int
            let sessaoAtualizada = Sessao id (getFilme s) horario dia tipo is3d sala (getAssentos s)
            editarSessao sessaoAtualizada sistemaRef
            
            