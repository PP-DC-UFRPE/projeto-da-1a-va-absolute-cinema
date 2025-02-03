module Pedidos where
import Tipos
import Dados
import Data.IORef
import Data.Traversable (for)
import System.IO
import Utils
import Control.Monad.RWS.Class (MonadState(put))


gerarIdPedido :: [Pedido] -> Id
gerarIdPedido pedidos = 
    if null pedidos 
        then 1 
        else maximum (map getIdPedido pedidos) + 1

exibirPedido :: Pedido -> IO ()
exibirPedido (Ped id (Cliente nome cpf _ _) (Sessao _ (Filme _ titulo _ _ _) (h, m) (d, mo, a) tipo _ sala _) ingressos valor) = do
    putStrLn $ "ID: " ++ show id
    putStrLn $ "Cliente: " ++ nome ++ " (CPF: " ++ cpf ++ ")"
    putStrLn $ "Filme: " ++ titulo
    putStrLn $ "Horário: " ++ show h ++ ":" ++ show m ++
               " | Dia: " ++ show d ++ "/" ++ show mo ++ "/" ++ show a
    putStrLn $ "Sala: " ++ show sala ++ " | " ++ show tipo
    putStrLn $ "Ingressos: " ++ show (length ingressos) ++ " - Valor total: R$ " ++ show valor
    mapM_ exibirIngresso ingressos
    putStrLn "________________________________________________________________________________"

exibirIngresso :: Ingresso -> IO ()
exibirIngresso (tipo, (letra, num, _)) = do
    putStrLn $ "  Assento: " ++ [letra] ++ show num ++ " | " ++ tipoToString tipo

tipoToString :: TipoIngresso -> String
tipoToString Inteira = "Inteira - R$ " ++ show valorInteira
tipoToString Meia        = "Meia - R$ " ++ show (valorInteira / 2)
printarIngresso :: Ingresso -> IO ()
printarIngresso (tipo, assento) = putStrLn $ formatarTipoIngresso tipo ++ " | Assento: " ++ formatarAssento assento

exibirTodosPedidos :: IORef Sistema -> IO ()
exibirTodosPedidos sistemaRef = do
    (_, _, _, pedidos) <- readIORef sistemaRef
    if null pedidos
        then putStrLn "\nNenhum pedido registrado."
        else do
            putStrLn "\n----- Pedidos -----"
            putStrLn ""
            mapM_ exibirPedido pedidos

removerPedido :: Id -> IORef Sistema -> IO ()
removerPedido id sistemaRef = do
    (clientes, filmes, sessoes, pedidos) <- readIORef sistemaRef
    let pedidosAtualizados = filter (\p -> getIdPedido p /= id) pedidos
    if length pedidosAtualizados == length pedidos
        then putStrLn "\nPedido não encontrado"
        else do 
            putStrLn "\nPedido removido com sucesso"
            writeIORef sistemaRef (clientes, filmes, sessoes, pedidosAtualizados)

menuRemoverPedido :: IORef Sistema -> IO ()
menuRemoverPedido sistemaRef = do
    putStrLn "----- Remover Pedido -----"
    putStrLn "Digite o ID do pedido que deseja remover"
    putStr "ID: "
    hFlush stdout
    input <- getLine
    let id = read input :: Id
    removerPedido id sistemaRef
