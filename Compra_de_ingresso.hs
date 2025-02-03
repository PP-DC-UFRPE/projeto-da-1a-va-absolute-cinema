module Compra_de_ingresso where
import Tipos
import System.IO
import Data.IORef
import Pedidos (exibirPedido, gerarIdPedido)
import Text.Read (readMaybe)
import Clientes
import Data.Maybe (isJust, fromJust)
import Control.Monad.RWS (MonadState(put))
import Sessoes (atualizarAssentosSessao, atualizarAssentos)
import Utils (formatarAssento)

gerarIngresso :: Assento -> IO Ingresso
gerarIngresso (l, n, ocupado) = do
    putStrLn $ "\nSelecione o tipo de ingresso do assento: " ++ show l ++ show n
    putStrLn "1 - Inteira"
    putStrLn "2 - Meia"
    putStr "Escolha: "
    hFlush stdout
    input <- getLine
    case input of
        "1" -> return (Inteira valorInteira, (l, n, ocupado))
        "2" -> return (Meia, (l, n, ocupado))
        _ -> do
            putStrLn "Tipo de ingresso inválido! Tente novamente."
            gerarIngresso (l, n, ocupado)

gerarIngressosPedido :: [Assento] -> IO [Ingresso]
gerarIngressosPedido assentos = mapM (\(l, n, ocupado) -> gerarIngresso (l, n, ocupado)) assentos

-- Atualização de assentos
atualizarAssento :: (Char, Int) -> [Assento] -> [Assento]
atualizarAssento (letra, num) assentos = [(l, n, if l == letra && n == num then True else ocupado) | (l, n, ocupado) <- assentos]

-- Verifica se o assento está disponível
verificaAssentoDisponivel :: (Char, Int) -> [Assento] -> Bool
verificaAssentoDisponivel (letra, num) assentos =
    not $ any (\(l, n, ocupado) -> l == letra && n == num && ocupado) assentos

inputAssento :: IO (Char, Int)
inputAssento = do
    putStr "\nLetra do assento (A-H): "
    hFlush stdout
    inputLetra <- getLine
    if head inputLetra `elem` ['A'..'H'] && length inputLetra == 1
        then do
            putStr "\nNúmero do assento (1-10): "
            hFlush stdout
            inputNum <- getLine
            let num = readMaybe inputNum :: Maybe Int
            if isJust num && fromJust num >= 1 && fromJust num <= 10
                then return (head inputLetra, fromJust num)
                else do
                    putStrLn "Número de assento inválido! Tente novamente."
                    inputAssento
        else do
            putStrLn "Letra de assento inválida! Tente novamente."
            inputAssento

escolherAssentos :: [Assento] -> IO [Assento]
escolherAssentos assentos = escolherAssentosAux assentos []

escolherAssentosAux :: [Assento] -> [Assento] -> IO [Assento]
escolherAssentosAux assentos escolhidos = do
    putStrLn "\nDigite 's' para selecionar um assento ou 'n' para finalizar a seleção."
    putStr "Escolha: "
    hFlush stdout
    input <- getChar
    _ <- getLine
    if input == 's' || input == 'S'
        then do
            (letra, num) <- inputAssento
            if verificaAssentoDisponivel (letra, num) assentos
                then do
                    putStrLn "\nAssento disponível, prosseguindo."
                    let assentoAtualizado = atualizarAssento (letra, num) assentos
                        assentoEscolhido = head $ filter (\(l, n, _) -> l == letra && n == num) assentoAtualizado
                    escolherAssentosAux assentoAtualizado (assentoEscolhido : escolhidos)
                else do
                    putStrLn "\nAssento ocupado! Tente outro assento."
                    escolherAssentosAux assentos escolhidos
        else return (reverse escolhidos)

-- Processo de compra de ingresso
compra :: IORef Sistema -> IO ()
compra sistemaRef = do
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, pedidos) = sistema
    putStrLn "\n----- Compra de Ingresso -----"
    putStrLn "Digite 's' para continuar ou qualquer outra tecla para cancelar."
    putStr "Escolha: "
    hFlush stdout
    input <- getChar
    _ <- getLine
    if input /= 's' && input /= 'S'
        then putStrLn "\nCompra cancelada."
        else do

        -- Seleciona o filme a ser assistido
        putStrLn "\nQual filme você quer assistir?"
        mapM_ (\(i, filme) -> putStrLn $ show i ++ ") " ++ pegarTitulo filme) (zip [0..] filmes)
        putStr "Digite o número (0 a n): "
        hFlush stdout
        input <- getLine
        let numFilme = case readMaybe input :: Maybe Int of
                Just n -> n
                Nothing -> -1
        if not (numFilme >= 0 && numFilme < length filmes)  -- Verifica se o número do filme é válido
            then do
                putStrLn "\nNúmero de filme inválido! Tente novamente."
                compra sistemaRef
            else do
                let filmeSelecionado = filmes !! numFilme
                    sessoesFilme = pegarSessoesPorFilme sessoes filmeSelecionado
                if null sessoesFilme
                    then do
                        putStrLn "\nNão há sessões disponíveis para este filme."
                        compra sistemaRef
                    else do
                        putStrLn "\nSessões disponíveis para este filme:"
                        mapM_ (\(i, Sessao _ _ (h, m) (d, mo, a) t _ sala _) ->
                            putStrLn $ show i ++ ") Sessão: " ++ show h ++ ":" ++ show m ++
                                    " - " ++ show d ++ "/" ++ show mo ++ "/" ++ show a ++
                                    " - " ++ show t ++ " - Sala " ++ show sala) (zip [0..] sessoesFilme)
                        putStr "Digite o número (0 a n): "
                        hFlush stdout
                        input <- getLine
                        let numSessao = case readMaybe input :: Maybe Int of
                                Just n -> n
                                Nothing -> -1
                        if not (numSessao >= 0 && numSessao < length sessoesFilme)  -- Verifica se o número da sessão é válido
                            then do
                                putStrLn "\nNúmero de sessão inválido! Tente novamente."
                                compra sistemaRef
                            else do
                                let sessaoSelecionada = sessoesFilme !! numSessao
                                    assentosSessao = getAssentos sessaoSelecionada
                                putStrLn "\nSessão selecionada."
                                putStrLn "\nEscolha os assentos desejados:"

                                printarAssentosDaSessao sessaoSelecionada
                                assentosEscolhidos <- escolherAssentos assentosSessao
                                putStrLn $ "\nAssentos selecionados: " ++ show assentosEscolhidos
                                
                                
                                let assentosAtualizados = atualizarAssentos (map (\(l, n, _) -> (l, n)) assentosEscolhidos) assentosSessao

                                sessoesAtualizada <- atualizarAssentosSessao (getIdSessao sessaoSelecionada) assentosAtualizados sessoes

                                putStrLn "\nAssentos selecionados:"
                                mapM_ (\(l, n, _) -> putStrLn $ "Assento: " ++ show l ++ show n) assentosEscolhidos

                                -- Determinar o tipo de ingresso pra cada assento
                                ingressos <- gerarIngressosPedido assentosEscolhidos

                                -- Calcular o valor total dos ingressos
                                let valorTotal = calcularValor ingressos
                                putStrLn $ "\nValor total: R$ " ++ show valorTotal

                                putStrLn "\nDigite 's' para confirmar a compra ou 'n' para cancelar."
                                putStr "Escolha: "
                                hFlush stdout
                                input <- getLine
                                if input == "s" || input == "S"
                                    then do
                                        putStrLn "\nPara continuar, insira seu CPF."
                                        putStr "CPF: "
                                        hFlush stdout
                                        cpf <- getLine
                                        let cliente = buscarCliente cpf clientes
                                        case cliente of
                                            Nothing -> do
                                                putStrLn "\nCliente não encontrado. Cadastre-se para continuar."
                                                menuCadastrarCliente sistemaRef
                                                compra sistemaRef
                                            Just c -> do
                                                putStrLn "\nCadastro identificado no sistema."
                                                putStrLn "\nFinalizando compra..."
                                                let novoPedido = Ped (gerarIdPedido pedidos) c sessaoSelecionada ingressos valorTotal
                                                writeIORef sistemaRef (clientes, filmes, sessoesAtualizada, novoPedido : pedidos)
                                                putStrLn "\nCompra finalizada! Pedido gerado com sucesso."
                                else putStrLn "\nCompra cancelada."                                         

-- Visualiza ingressos comprados
visualizarIngressos :: IORef Sistema -> IO ()
visualizarIngressos sistemaRef = do
    pedidos <- pegarPedidos sistemaRef
    if null pedidos
        then putStrLn "Nenhum ingresso foi comprado ainda.\n"
        else do
            putStrLn "Ingressos comprados:\n"
            mapM_ exibirPedido pedidos
            putStrLn ""
