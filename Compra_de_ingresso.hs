module Compra_de_ingresso where
import Dados
import Tipos
import System.IO
import Data.IORef

compra :: IORef Sistema -> IO ()
compra sistemaRef = do
    putStrLn "Você deseja ver a lista de filmes?"
    putStr "Digite 's' se sim e 'n' se nao: "
    hFlush stdout
    input <- getChar
    _ <- getLine
    sistema <- readIORef sistemaRef
    let (clientes, filmes, sessoes, pedidos) = sistema
    if input == 's' then printarFilmesESessoes sistema else return ()
    
    putStrLn "Qual filme você quer assistir?"
    putStr "Digite o numero (0 a n): "
    hFlush stdout
    input <- getLine
    let num = read input :: Int

    printarSessoesPorFilme sessoes (filmes !! num)

    putStr "Digite o numero da sala: "
    hFlush stdout
    salaInput <- getLine
    let salaNum = read salaInput :: Int

    putStrLn ""
    printarAssentosPorNumeroSessao salaNum sessoes

    putStr "Letra do assento: "
    hFlush stdout
    letra <- getChar
    _ <- getLine

    putStr "Numero do assento: "
    hFlush stdout
    assentoInput <- getLine
    let numAssento = read assentoInput :: Int

    let novasSessoes = atualizarAssento letra numAssento sessoes
        novoSistema = (clientes, filmes, novasSessoes, pedidos)

    writeIORef sistemaRef novoSistema
    putStrLn "Assento atualizado com sucesso!"
