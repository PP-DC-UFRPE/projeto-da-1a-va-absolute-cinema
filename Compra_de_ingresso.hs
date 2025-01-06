module Compra_de_ingresso where
import Dados
import Tipos
import System.IO
import Data.IORef

-- Processo de compra de ingresso
compra :: IORef Sistema -> IO ()
compra sistemaRef = do
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, pedidos) = sistema

    -- Pergunta se o usuário deseja ver a lista de filmes
    putStrLn "Você deseja ver a lista de filmes?"
    putStr "Digite 's' se sim e 'n' se não: "
    hFlush stdout
    input <- getChar
    _ <- getLine
    if input == 's' then printarFilmesESessoes sistema else return ()

    -- Seleciona o filme a ser assistido
    putStrLn "Qual filme você quer assistir?"
    putStr "Digite o número (0 a n): "
    hFlush stdout
    input <- getLine
    let numFilme = read input :: Int
    if numFilme >= 0 && numFilme < length filmes  -- Verifica se o número do filme é válido
        then do
            let filmeSelecionado = filmes !! numFilme
            printarSessoesPorFilme sessoes filmeSelecionado  -- Exibe as sessões do filme
        else do
            putStrLn "Número de filme inválido! Tente novamente."

    -- Seleciona a sala do filme
    putStr "Digite o número da sala: "
    hFlush stdout
    salaInput <- getLine
    let salaNum = read salaInput :: Int
    if salaNum >= 0  -- Verifica se o número da sala é válido
        then printarAssentosPorNumeroSessao salaNum sessoes
        else putStrLn "Número de sala inválido. Tente novamente."

    -- Seleciona o assento
    putStr "Letra do assento: "
    hFlush stdout
    letra <- getChar
    _ <- getLine
    putStr "Número do assento: "
    hFlush stdout
    assentoInput <- getLine
    let numAssento = read assentoInput :: Int

    -- Verifica se o assento está disponível
    let sessaoSelecionada = head (filter (\(Sessao _ _ _ _ n _) -> n == salaNum) sessoes)
    let assentoDisponivel = verificaAssentoDisponivel letra numAssento sessaoSelecionada

    if assentoDisponivel
        then do
            putStrLn "Assento disponível, prosseguindo"
            -- Processo de cadastro do cliente
            putStrLn "Informe seus dados para cadastro:"
            putStr "Nome: "
            hFlush stdout
            nome <- getLine
            putStr "CPF: "
            hFlush stdout
            cpf <- getLine
            putStr "Idade: "
            hFlush stdout
            idadeInput <- getLine
            let idade = read idadeInput :: Int
            if idade >= 18  -- Verifica se a idade é válida
                then return ()  -- Continua se a idade for válida
                else putStrLn "Idade inválida."
            
            -- Coleta ocupação do cliente
            putStrLn "Ocupação: (1 - Estudante, 2 - Professor, 3 - Outras)"
            putStr "Escolha: "
            hFlush stdout
            ocupInput <- getLine
            let ocupacao = case ocupInput of
                    "1" -> Estudante
                    "2" -> Professor
                    _   -> Outras
            let cliente = Cliente nome cpf idade ocupacao

            -- Determina o tipo de ingresso
            putStrLn "Tipo de ingresso: (1 - Inteira, 2 - Meia)"
            putStr "Escolha: "
            hFlush stdout
            ingressoInput <- getLine
            let tipoIngresso = case ingressoInput of
                    "1" -> Inteira 20.0  -- Preço de ingresso inteiro
                    _   -> Meia  -- Preço de meia-entrada

            -- Atualiza o sistema com o novo ingresso
            let novoIngresso = (tipoIngresso, (letra, numAssento, True))
                pedido = Ped cliente sessaoSelecionada [novoIngresso] (calcularValor [novoIngresso])
                novasSessoes = atualizarAssento letra numAssento sessoes
                novoSistema = (clientes ++ [cliente], filmes, novasSessoes, pedidos ++ [pedido])

            writeIORef sistemaRef novoSistema
            putStrLn "Compra finalizada! Ingresso gerado com sucesso."
        else putStrLn "Assento ocupado! Tente outro assento."

-- Verifica se o assento está disponível
verificaAssentoDisponivel :: Char -> Int -> Sessao -> Bool
verificaAssentoDisponivel letra numAssento (Sessao _ _ _ _ _ assentos) =
    not $ any (\(l, n, ocupado) -> l == letra && n == numAssento && ocupado) assentos

-- Visualiza ingressos comprados
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
