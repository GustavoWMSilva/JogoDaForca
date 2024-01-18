------------------------------------------------------------------------------------------------------------------------
-- Grupo: Gabriel Zurawski de Souza, Gustavo Willian Martins da Silva, Lucas Schwartz dos Santos, Vinícius Pereira Dias
-- Para rodar nas nossas máquinas usamos: ghci -package clock forca.hs
-- Nova mecânica: O tempo para dar uma letra é proporcional a quantia de tentativas que o jogador ainda tem.
------------------------------------------------------------------------------------------------------------------------

import System.IO
import System.Random
import System.Clock 
import Text.Printf (printf)
import System.Process (system)
import Control.Concurrent (putMVar)
import Data.Binary.Builder (putInt16be, putStringUtf8)
import GHC.Conc (threadDelay)
import Control.Concurrent 
import Control.Concurrent.MVar
import Control.Monad (when)

main = forca

forca :: IO ()
forca = do
    limparTela
    palavra <- escolhePalavra
    play palavra "" 6

escolhePalavra :: IO String
escolhePalavra = do
    palavras <- leArquivo 
    numeroAleatorio <- randomRIO (0, length palavras - 1)
    return (palavras !! numeroAleatorio) 

leArquivo :: IO [String]
leArquivo = do
    arquivo <- readFile "palavras.txt"
    let palavras = lines arquivo
    return palavras

play :: String -> String -> Int -> IO ()
play palavra historico tentativas = do
    putStrLn (match palavra historico)
    putStr "Letras digitadas: "
    putStrLn (letrasDigitadas historico)
    desenhaForca tentativas
    putStr "Tentativas restantes: "
    putStrLn (show tentativas)
    if match palavra historico == palavra then do
        limparTela
        putStrLn "Parabéns, você ganhou! (Todas as letras para a nova palavra já foram digitadas!)"
        continua
    else do
        -- Fim de jogo
        if tentativas == 0 then do
            putStrLn "A palavra era: "
            putStrLn palavra
            continua
        else do
            -- Clcula a chance de gerar uma nova palavra
            numeroAleatorio <- randomRIO (0.1, 1.0) :: IO Float
            if ((contaDiferenca palavra historico) == 1) && (numeroAleatorio < 0.3) then do
                novaPalavra <- escolhePalavra
                limparTela
                putStrLn "A palavra foi trocada!"
                threadDelay 1500000
                limparTela
                play novaPalavra historico tentativas
            else do
                putStr "Digite uma letra: "
                hSetBuffering stdout NoBuffering
                tempoAcabou <- newEmptyMVar
                putStrLn ("Você tem " ++ show (tentativas*2) ++ " segundos para responder...")
                forkIO (cronometro (tentativas*2) tempoAcabou)
                letra <- getLine
                tempoFim <- tryTakeMVar tempoAcabou
                -- Verifica se o tempo acabou
                case tempoFim of
                    Just _ -> do
                        limparTela
                        play palavra historico (tentativas - 1)
                    Nothing -> do
                        putMVar tempoAcabou False
                        -- Verifica se entrada do terminal digitada é válida
                        if elem letra alfabeto == False then do
                            limparTela
                            putStrLn (match palavra (concatena historico letra))
                            putStrLn "Caractere Inválido!"
                            threadDelay 1500000
                            limparTela
                            play palavra historico tentativas
                        else do
                            -- Verifica se a letra já foi digitada
                            if elem (head letra) historico then do
                                limparTela
                                putStrLn (match palavra (concatena historico letra))
                                putStrLn "Letra já digitada!"
                                threadDelay 1500000
                                limparTela
                                play palavra historico tentativas
                            else do
                                -- Verifica se a letra pretence a palavra
                                if elem (head letra) palavra then do
                                    -- Verifica se o jogador ganhou
                                    if match palavra (concatena historico letra) == palavra then do
                                        limparTela
                                        putStrLn "Parabéns, você ganhou!"
                                        continua
                                    else do
                                        limparTela
                                        play palavra (concatena historico letra) tentativas
                                else do   
                                    limparTela
                                    play palavra (concatena historico letra) (tentativas - 1)

contaDiferenca :: String -> String -> Int
contaDiferenca palavra historico = length [x | x <- palavra, notElem x historico]

continua :: IO ()
continua = do
    putStrLn "Quer jogar de novo?"
    putStrLn "s/n"
    resposta <- getLine
    case resposta of
        ('s':_) -> do
            limparTela
            forca
        ('n':_) -> do
            limparTela
            putStrLn "Obrigado por jogar!"
        _ -> do
            limparTela
            putStrLn "Resposta Inválida!"
            threadDelay 1500000
            continua


sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n' then do
        putChar x
        return []
    else do
        putChar '-'
        xs <- sgetLine
        return (x:xs)

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

alfabeto :: [String]
alfabeto = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
             "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
             "u", "v", "w", "x", "y", "z"]
    
cronometro :: Int -> MVar Bool -> IO ()
cronometro 0 mvar = do
    limparTela
    putStrLn "\nTempo esgotado!"
    tryPutMVar mvar True 
    return ()
cronometro n mvar = do
    threadDelay 1000000
    fim <- tryTakeMVar mvar  
    case fim of
        Just _ -> return ()  
        Nothing -> cronometro (n-1) mvar


match :: String -> String -> String
match palavra chute = [if elem x chute then x else '-' | x <- palavra ]

letrasDigitadas :: String -> String
letrasDigitadas historico = [if elem x historico then x else ' ' | x <- "abcdefghijklmnopqrstuvwxyz" ]

concatena :: String -> String -> String
concatena historico letra = historico ++ letra

limparTela :: IO ()
limparTela = do
    _ <- system "cls"
    return ()

desenhaForca :: Int -> IO ()
desenhaForca 6 = do
    putStrLn "_________     "
    putStrLn "|       |     "
    putStrLn "|       O     "
    putStrLn "|      /|\\   "
    putStrLn "|      / \\   "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "=============="
desenhaForca 5 = do
    putStrLn "_________     "
    putStrLn "|       |     "
    putStrLn "|       O     "
    putStrLn "|      /|\\   "
    putStrLn "|      /      "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "=============="
desenhaForca 4 = do
    putStrLn "_________     "
    putStrLn "|       |     "
    putStrLn "|       O     "
    putStrLn "|      /|\\   "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "=============="
desenhaForca 3 = do
    putStrLn "_________     "
    putStrLn "|       |     "
    putStrLn "|       O     "
    putStrLn "|      /|     "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "=============="
desenhaForca 2 = do
    putStrLn "_________     "
    putStrLn "|       |     "
    putStrLn "|       O     "
    putStrLn "|       |     "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "=============="
desenhaForca 1 = do
    putStrLn "_________     "
    putStrLn "|       |     "
    putStrLn "|       O     "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "|             "
    putStrLn "=============="
desenhaForca 0 = do
    putStrLn "_________             "
    putStrLn "|       |             "
    putStrLn "|                     "
    putStrLn "|     Fim de Jogo     "
    putStrLn "|                     "
    putStrLn "|                     "
    putStrLn "|                     "
    putStrLn "==============        "
