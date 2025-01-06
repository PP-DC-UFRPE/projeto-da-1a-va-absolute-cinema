module Compra_de_ingresso where
import Dados
import Tipos
import System.IO
import Data.IORef

compra :: IORef Sistema -> IO ()
compra sistemaRef = do
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, pedidos) = sistema

    -- Passo 1: Pergunta se o usuário deseja ver a lista de filmes
    putStrLn "Você deseja ver a lista de filmes?"
    putStr "Digite 's' se sim e 'n' se não: "
    hFlush stdout
    input <- getChar
    _ <- getLine
    if input == 's' then printarFilmesESessoes sistema else return ()

    -- Passo 2: Selecionar o filme
    putStrLn "Qual filme você quer assistir?"
    putStr "Digite o número (0 a n): "
    hFlush stdout
    input <- getLine
    let numFilme = read input :: Int
    let filmeSelecionado = filmes !! numFilme
    printarSessoesPorFilme sessoes filmeSelecionado

    -- Passo 3: Selecionar uma sessão
    putStr "Digite o número da sala: "
    hFlush stdout
    salaInput <- getLine
    let salaNum = read salaInput :: Int
    putStrLn ""
    printarAssentosPorNumeroSessao salaNum sessoes

    -- Passo 4: Selecionar um assento
    putStr "Letra do assento: "
    hFlush stdout
    letra <- getChar
    _ <- getLine
    putStr "Número do assento: "
    hFlush stdout
    assentoInput <- getLine
    let numAssento = read assentoInput :: Int

    -- Passo 5: Coletar os dados do usuário
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
    putStrLn "Ocupação: (1 - Estudante, 2 - Professor, 3 - Outras)"
    putStr "Escolha: "
    hFlush stdout
    ocupInput <- getLine
    let ocupacao = case ocupInput of
            "1" -> Estudante
            "2" -> Professor
            _   -> Outras
    let cliente = Cliente nome cpf idade ocupacao

    -- Passo 6: Determinar o tipo de ingresso
    putStrLn "Tipo de ingresso: (1 - Inteira, 2 - Meia)"
    putStr "Escolha: "
    hFlush stdout
    ingressoInput <- getLine
    let tipoIngresso = case ingressoInput of
            "1" -> Inteira 20.0 -- Exemplo de preço
            _   -> Meia

    -- Passo 7: Atualizar o sistema
    let novoIngresso = (tipoIngresso, (letra, numAssento, True))
        pedido = Ped cliente (head (filter (\(Sessao _ _ _ _ n _) -> n == salaNum) sessoes)) [novoIngresso] (calcularValor [novoIngresso])
        novasSessoes = atualizarAssento letra numAssento sessoes
        novoSistema = (clientes ++ [cliente], filmes, novasSessoes, pedidos ++ [pedido])

    writeIORef sistemaRef novoSistema
    putStrLn "Compra finalizada! Ingresso gerado com sucesso."

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
