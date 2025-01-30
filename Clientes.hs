module Clientes where

import Tipos
import Dados
import Data.IORef
import System.IO
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Type.Bool (Not)

-- Formata um cliente para exibição
printarCliente :: Cliente -> IO ()
printarCliente (Cliente nome cpf idade ocupacao) = do
    putStrLn $ nome ++ " | " ++ cpf ++ " | " ++ show idade ++ " | " ++ show ocupacao
    putStrLn "_______________________________________________________"

-- Exibe todos os clientes cadastrados
exibirClientes :: IORef Sistema -> IO ()
exibirClientes sistemaRef = do
    clientes <- pegarClientes sistemaRef
    putStrLn "----- Exibir Clientes -----"
    putStrLn "Nome               | CPF          | Idade | Ocupação"
    putStrLn "-------------------------------------------------------"
    mapM_ printarCliente clientes
    putStrLn ""

buscarCliente :: Cpf -> [Cliente] -> Maybe Cliente
buscarCliente _ [] = Nothing
buscarCliente cpf (c:cs) = if getCpf c == cpf then Just c else buscarCliente cpf cs

cadastrarCliente :: Cliente -> IORef Sistema -> IO ()
cadastrarCliente cliente sistemaRef = do
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    writeIORef sistemaRef (cliente:clientes, filmes, sessoes, pedidos)

removerCliente :: Cpf -> IORef Sistema -> IO ()
removerCliente cpf sistemaRef = do
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let clientesAtualizados = filter (\c-> getCpf c /= cpf) clientes
    if length clientesAtualizados == length clientes
        then putStrLn "\nCliente não encontrado"
        else do
            putStrLn "\nCliente removido com sucesso"
            writeIORef sistemaRef (clientesAtualizados, filmes, sessoes, pedidos)

editarCliente :: Cliente -> IORef Sistema -> IO ()
editarCliente cliente sistemaRef = do
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let clientesAtualizados = map (\c -> if getCpf c == getCpf cliente then cliente else c) clientes
    writeIORef sistemaRef (clientesAtualizados, filmes, sessoes, pedidos)

validarNome :: String -> Bool
validarNome "" = False
validarNome nome = length (words nome) >= 2

validarCpf :: Cpf -> Bool
validarCpf "" = False
validarCpf cpf = length cpf == 11 && all isDigit cpf

validarIdade :: Int -> Bool
validarIdade idade = idade >= 18

validarCliente :: Cliente -> Bool
validarCliente (Cliente nome cpf idade ocupacao) = validarNome nome && validarCpf cpf && validarIdade idade

menuCadastrarCliente :: IORef Sistema -> IO ()
menuCadastrarCliente sistemaRef = do
    (clientes, _, _, _) <- readIORef sistemaRef
    putStrLn "----- Cadastrar Cliente -----"
    putStr "Nome Completo: "
    hFlush stdout
    nome <- getLine
    putStr "\nCPF (somente digitos): "
    hFlush stdout
    cpf <- getLine
    putStr "\nIdade (maior de 18): "
    hFlush stdout
    idadeIpt <- getLine
    let idade = case idadeIpt of
            "" -> 0
            _  -> read idadeIpt :: Int

    putStrLn "\nOcupação: (1 - Estudante, 2 - Professor, 3 - Outras)"
    putStr "Escolha: "
    hFlush stdout
    ocupInput <- getLine
    let ocupacao = case ocupInput of
            "1" -> Estudante
            "2" -> Professor
            _   -> Outras

    if not (validarNome nome)
        then putStrLn "\nErro: o nome deve conter um sobrenome"
            else if not (validarCpf cpf)
                then putStrLn "\nErro: CPF inválido. Deve conter 11 dígitos"
                    else if not (validarIdade idade)
                        then putStrLn "\nErro: cliente deve ser maior de 18 anos"
                            else do
                                if isJust (buscarCliente cpf clientes)
                                    then putStrLn "\nErro: CPF já cadastrado"
                                    else do
                                    let cliente = Cliente nome cpf idade ocupacao
                                    cadastrarCliente cliente sistemaRef
                                    putStrLn "\nCliente cadastrado com sucesso"

menuRemoverCliente :: IORef Sistema -> IO ()
menuRemoverCliente sistemaRef = do
    putStrLn "----- Remover Cliente -----"
    putStrLn "Digite o CPF do cliente que deseja remover"
    putStr "CPF: "
    hFlush stdout
    cpf <- getLine
    removerCliente cpf sistemaRef

menuEditarCliente :: IORef Sistema -> IO ()
menuEditarCliente sistemaRef = do
    (clientes, _, _, _) <- readIORef sistemaRef
    putStrLn "----- Editar Cliente -----"
    putStrLn "Digite o CPF do cliente que deseja editar"
    putStr "CPF: "
    hFlush stdout
    cpf <- getLine
    let cliente = buscarCliente cpf clientes
    case cliente of
        Nothing -> putStrLn "\nCliente não encontrado"
        Just c -> do
            putStrLn "\nCliente encontrado: "
            printarCliente c
            putStrLn "\nNão digite nada nos campos que deseja manter o mesmo"
            putStrLn "\nDigite o novo nome do cliente"
            putStr "Nome: "
            hFlush stdout
            nomeIpt <- getLine
            let nome = case nomeIpt of
                    "" -> getNome c
                    _  -> nomeIpt

            putStrLn "\nDigite a nova idade do cliente"
            putStr "Idade: "
            hFlush stdout
            idadeIpt <- getLine
            let idade = case idadeIpt of
                    "" -> getIdade c
                    _  -> read idadeIpt :: Int

            putStrLn "\nDigite a nova ocupação do cliente (1 - Estudante, 2 - Professor, 3 - Outras)"
            putStr "Ocupação: "
            hFlush stdout
            ocupInput <- getLine
            let ocupacao = case ocupInput of
                    "1" -> Estudante
                    "2" -> Professor
                    "3"  -> Outras
                    _ -> getOcupacao c

            if not (validarNome nome)
                then putStrLn "\nErro: o nome deve conter um sobrenome"
                    else if not (validarIdade idade)
                        then putStrLn "\nErro: cliente deve ser maior de 18 anos"
                            else do
                                let clienteAtualizado = Cliente nome cpf idade ocupacao
                                editarCliente clienteAtualizado sistemaRef
                                putStrLn "\nCliente editado com sucesso"