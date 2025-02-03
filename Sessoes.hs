module Sessoes where
import Tipos
import Dados
import Utils
import Data.IORef
import Data.Traversable (for)
import System.IO
import Text.Read (readMaybe)
import Control.Monad.RWS (MonadState(put))

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
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    writeIORef sistemaRef (clientes, filmes, sessao:sessoes, pedidos)

removerSessao :: Id -> IORef Sistema -> IO ()
removerSessao id sistemaRef = do
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let sessoesAtualizadas = filter (\s -> getIdSessao s /= id) sessoes
    if length sessoesAtualizadas == length sessoes
        then putStrLn "\nSessão não encontrada"
        else do
            putStrLn "\nSessão removida com sucesso"
            writeIORef sistemaRef (clientes, filmes, sessoesAtualizadas, pedidos)

editarSessao :: Sessao -> IORef Sistema -> IO ()
editarSessao sessao sistemaRef = do
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let sessoesAtualizadas = map (\s -> if getIdSessao s == getIdSessao sessao then sessao else s) sessoes
    writeIORef sistemaRef (clientes, filmes, sessoesAtualizadas, pedidos)
    putStrLn "\nSessao editada com sucesso"

atualizarAssentos :: [(Char, Int)] -> [Assento] -> [Assento]
atualizarAssentos [] assentos = assentos
atualizarAssentos ((l, n):resto) assentos = atualizarAssentos resto [(letra, num, (letra == l && num == n) || ocupado) | (letra, num, ocupado) <- assentos]
    
atualizarAssentosSessao :: Id -> [Assento] -> [Sessao] -> IO [Sessao]
atualizarAssentosSessao id novosAssentos sessoes = do
    let sessao = buscarSessao id sessoes
    case sessao of
        Nothing -> return sessoes
        Just s -> do
            let sessaoAtualizada = s { getAssentos = novosAssentos }
            putStrLn $ show sessaoAtualizada
            putStrLn $ show (map (\sessao -> if getIdSessao sessao == id then sessaoAtualizada else sessao) sessoes)
            return $ map (\sessao -> if getIdSessao sessao == id then sessaoAtualizada else sessao) sessoes

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

inputHora :: IO Int
inputHora = do
    putStr "\nHH: "
    hFlush stdout
    iptHora <- getLine
    case validarIptHora iptHora of
        Right hora -> return hora
        Left erro -> do
            putStrLn erro
            inputHora

validarIptHora :: String -> Either String Int
validarIptHora input = do
    let ipt = readMaybe input :: Maybe Int
    case ipt of
        Just i | i <= 23 && i >= 0 -> Right i
        Just _ -> Left "\nHora inválida: deve estar entre 0 e 23."
        Nothing -> Left "\nInput inválido: digite um valor inteiro entre 0 e 23."

inputMinuto :: IO Int
inputMinuto = do
    putStr "\nMM: "
    hFlush stdout
    iptMinuto <- getLine
    case validarIptMinuto iptMinuto of
        Right minuto -> return minuto
        Left erro -> do
            putStrLn erro
            inputMinuto

validarIptMinuto :: String -> Either String Int
validarIptMinuto input = do
    let ipt = readMaybe input :: Maybe Int
    case ipt of
        Just i | i <= 59 && i >= 0 -> Right i
        Just _ -> Left "\nMinuto inválido: deve estar entre 0 e 59."
        Nothing -> Left "\nInput inválido: digite um valor inteiro entre 0 e 59."

inputDia :: IO Int
inputDia = do
    putStr "\nDia: "
    hFlush stdout
    iptDia <- getLine
    case validarIptDia iptDia of
        Right dia -> return dia
        Left erro -> do
            putStrLn erro
            inputDia

validarIptDia :: String -> Either String Int
validarIptDia input = do
    let ipt = readMaybe input :: Maybe Int
    case ipt of
        Just i | i <= 31 && i >= 1 -> Right i
        Just _ -> Left "\nDia inválido: deve estar entre 1 e 31."
        Nothing -> Left "\nInput inválido: digite um valor inteiro entre 1 e 31."

inputMes :: IO Int
inputMes = do
    putStr "\nMês: "
    hFlush stdout
    iptMes <- getLine
    case validarIptMes iptMes of
        Right mes -> return mes
        Left erro -> do
            putStrLn erro
            inputMes

validarIptMes :: String -> Either String Int
validarIptMes input = do
    let ipt = readMaybe input :: Maybe Int
    case ipt of
        Just i | i <= 12 && i >= 1 -> Right i
        Just _ -> Left "\nMês inválido: deve estar entre 1 e 12."
        Nothing -> Left "\nInput inválido: digite um valor inteiro entre 1 e 12."

inputAno :: IO Int
inputAno = do
    putStr "\nAno: "
    hFlush stdout
    iptAno <- getLine
    case validarIptAno iptAno of
        Right ano -> return ano
        Left erro -> do
            putStrLn erro
            inputAno

validarIptAno :: String -> Either String Int
validarIptAno input = do
    let ipt = readMaybe input :: Maybe Int
    case ipt of
        Just i | i > 2024 -> Right i
        Just _ -> Left "\nAno inválido: deve ser maior que 2024."
        Nothing -> Left "\nInput inválido: digite um valor inteiro maior que 2024."

validarSessao :: Sessao -> [Sessao] -> Either String Sessao
validarSessao sessao sessoes = 
    if any (\s -> getHorario s == getHorario sessao && getDia s == getDia sessao && getSala s == getSala sessao) sessoes
        then Left "\nConflito: já existe uma sessão no mesmo horário, dia e sala."
        else Right sessao

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
        putStrLn "\nHorário (HH:MM)"
        hh <- inputHora
        mm <- inputMinuto
        let horario = (hh, mm)

        putStrLn "\nData (DD/MM/AAAA)"
        dd <- inputDia
        mm <- inputMes
        aaaa <- inputAno
        let dia = (dd, mm, aaaa)

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
        let sessao = Sessao (gerarIdSessao sessoes) filme horario dia tipo is3d sala assentos
        case validarSessao sessao sessoes of
            Right sessao -> do 
                adicionarSessao sessao sistemaRef
                putStrLn "\nSessao adicionada com sucesso!"
            Left erro -> putStrLn erro
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

