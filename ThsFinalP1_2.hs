
import System.IO
import Data.List (delete)
import Data.List (nub)
import Control.Monad
import Data.List
import System.Environment

main :: IO()
main = do                                                                                           
        putStrLn "1 - Tarefa 1-Todos Alunos nas Uc"
        putStrLn "2 - Tarefa 2-Todas as Ucs de cada Aluno"
        putStrLn "3 - Tarefa 3-Filtar por Uc"
        putStrLn "4 - Tarefa 4-Filtrar por Aluno"
        putStrLn "5 - Parte 2"
        opcao <- getLine
        chamaopcao opcao

chamaopcao :: String -> IO()
chamaopcao opcao      
            | opcao == "1" = tarefa1                                                                        
            | opcao == "2" = tarefa2
            | opcao == "3" = le_ucs2
            | opcao == "4" = le_inscricoes
            | opcao == "5" = main_2
            | otherwise = error "Opcao invalida"

main_2 :: IO()                                                                                      
main_2 = do
        putStrLn "Escolha uma opção:"
        putStrLn "1 - Tarefa 1-Ficheiro de escalonamento sem restrições"
        putStrLn "2 - Tarefa 2-Escalonamento com restrição de só um exame de um ano por dia"
        putStrLn "3 - Tarefa 3-Incompatibilidades entre pares de UCs"
        putStrLn "4 - Tarefa 4-Adicionar ao ficheiro da tarefa 1 as incompatibilidades que apresenta"
        putStrLn "5 - Tarefa 5-Minimização de incompatibilidades"
        putStrLn "6 - Tarefa 6-Escalonamento com salas com lotação limitada"
        opcao2 <- getLine
        chamaopcao2 opcao2

chamaopcao2 :: String -> IO()
chamaopcao2 opcao2
            | opcao2 == "1" = tarefa1_p2
            | opcao2 == "2" = tarefa2_p2
            | opcao2 == "3" = tarefa3_p2
            | opcao2 == "4" = tarefa4_p2
            | opcao2 == "5" = tarefa5_p2
            | opcao2 == "6" = tarefa6_p2
            | otherwise = error "Opcao invalida"

le_ucs2 :: IO()
le_ucs2 = do                                                                                        
            conteudo <- readFile "ucs.txt"
            putStrLn "Insira um numero de uma UC (1 a 4)"
            putStrLn "1 - Algebra Linear"
            putStrLn "2 - Analise Matemática I"
            putStrLn "3 - Laboratorio de informatica e computadores"
            putStrLn "4 - Topicos de matematica discreta"
            putStrLn "5 - Programacao procedimental"
            putStrLn "6 - Seminario de informatica"
            putStrLn "7 - Analise matematica II"
            putStrLn "8 - Elementos de Física Geral"
            putStrLn "9 - Engenharia nas organizacoes"
            putStrLn "10 - Laboratorio de programacao"
            putStrLn "11 - Programacao funcional"
            putStrLn "12 - Sistemas computacionais"
            putStrLn "13 - Compiladores"
            putStrLn "14 - Engenharia de software"
            putStrLn "15 - Métodos estatísticos"
            putStrLn "16 - Programacao orientada a objetos"
            putStrLn "17 - Sistemas operativos"
            putStrLn "18 - Algoritmia e estruturas de dados"
            putStrLn "19 - Bases de dados"
            putStrLn "20 - Comunicacao de dados"
            putStrLn "21 - Laboratorio de planeamento e desenvolvimento de software"
            putStrLn "22 - Metodos computacionais em engenharia"
            putStrLn "23 - Programacao multiplataforma"
            putStrLn "24 - Engenharia web"
            putStrLn "25 - Gestao de projetos em engenharia"
            putStrLn "26 - Inteligencia artificial"
            putStrLn "27 - Laboratorio de aplicacoes web e bases de dados"
            putStrLn "28 - Redes de computadores"
            putStrLn "29 - Sistemas de informacao"
            putStrLn "30 - Introducao a ciencia dos dados"
            putStrLn "31 - Computacao grafica"
            putStrLn "32 - Interacao pessoa computador"
            putStrLn "33 - Laboratorio de projeto em engenharia informatica"
            putStrLn "34 - Sistemas distribuidos"
            uc <- getLine 
            conteudoAlunos <- readFile "listaalunos.txt"
            filtro_uc uc (lines conteudo)
            inscricoes <- readFile "inscricoes.txt"
            check_al (lines inscricoes)
            filtro_ano_nome uc (lines conteudoAlunos)
        


printalAluno :: String -> [String] -> IO()
printalAluno al [] = return()
printalAluno al (li:lis) = 
                            if head(words li) == al then do
                                    imprime_uc (words li)
                                else printalAluno al lis

check_al :: [String] -> IO()
check_al [] = return()
check_al (linha:linhas) = do
            contentAluno <- readFile "listaalunos.txt"
            printalAluno al (lines contentAluno)
            check_al linhas
            where al = head (words linha)

filtro_uc :: String -> [String] -> IO()
filtro_uc uc [] = return()                                                             
filtro_uc uc(linha:linhas) = do
                if head(words linha) == uc then 
                            imprime_uc (words linha)
                    else filtro_uc uc linhas
                    
imprime_uc :: [String] -> IO()
imprime_uc (x:xs:xy) = do
                            putStrLn (unwords xy)
            
filtro_ano_nome :: String -> [String] -> IO ()
filtro_ano_nome uc [] = return ()                                                                   
filtro_ano_nome uc (li : lis) = do
        let ws = words li
        if length ws >= 4 && ws !! 1 == uc
        then do
            putStrLn (ws !! 2 ++ " " ++ ws !! 3)
            filtro_ano_nome uc lis
        else
            filtro_ano_nome uc lis

le_inscricoes :: IO()
le_inscricoes = do                                                                                  
            nome <- getLine 
            inscricoes <- readFile "listaalunos.txt"
            show_name_al nome (lines inscricoes)
            

filtra_al :: String -> [String] -> IO()
filtra_al al [] = return ()                                                                         
filtra_al al (lis:list) = do
                                        if head(words lis) == al then do
                                                                            show_uc_aluno (last (words lis)) 
                                                                            filtra_al al list

                                        else filtra_al al list

show_uc_aluno :: String -> IO()
show_uc_aluno uc = do                                                                               
                        ucs_file <- readFile "ucs.txt"
                        filtro_uc uc (lines ucs_file)

tarefa1 :: IO a
tarefa1 = do                                                                                        
            conteudo <- readFile "ucs.txt" 
            show_uc (lines conteudo)


show_uc :: [String] -> IO a
show_uc (linha:linhas) = do                                                                         
                            if last(words linha) == "funcional" then putStrLn ((words linha !! 2) ++ " " ++ (last(words linha)))
                                else putStrLn (words linha !! 2)
                            show_nome (head(words linha))
                            show_uc linhas
                            

show_nome :: String -> IO ()
show_nome uc = do                                                                                   
                conteudoAlunos2 <- readFile "listaalunos.txt"
                filtro_ano_nome uc (lines(conteudoAlunos2))

tarefa2 :: IO a
tarefa2 = do
            conteudoAlunos3 <- readFile "listaalunos.txt"
            prato <- readFile "inscricoes.txt"
            show_name (lines conteudoAlunos3)
            

show_name :: [String] -> IO a
show_name (name:names)= do                                                                         
                            putStrLn((words name !! 2) ++ " " ++ (words name !! 3)) 
                            insclis <- readFile "inscricoes.txt"
                            filtra_al (head (words name)) (lines insclis)
                            show_name names

show_name_al :: String -> [String] -> IO()
show_name_al nome [] = error "Aluno não existe"                                                     
show_name_al nome (incri:incricos) = do
    if ((words incri) !! 2) == nome then do
        putStrLn (((words incri) !! 2) ++ " " ++ ((words incri) !! 3))
        inscricoes <- readFile "inscricoes.txt"
        filtra_al (head (words incri)) (lines inscricoes)
        else show_name_al nome incricos
                                    
-- Parte2
tarefa1_p2:: IO()                      
tarefa1_p2 = do
        conteudoDisciplina <- readFile "ucs.txt"
        ficheiro <- openFile "Tarefa1_p2.txt" WriteMode
        hClose ficheiro

        putStrLn "Quantos dias podem ecorrer exames"
        dias <- getLine
        putStrLn "Quantas salas estão disponiveis por dia"
        salas <- getLine

        let numDisciplinas = length(lines conteudoDisciplina)
        let numDias = read dias :: Int
        let numSalas = read salas :: Int
        if numDisciplinas > numDias * numSalas
            then putStrLn "Dias ou Salas insuficientes para todos os exames"
            else atribuiSalasDias 1 numSalas (lines conteudoDisciplina)

atribuiSalasDias :: Int -> Int -> [String] -> IO()                   
atribuiSalasDias numDias numSalas [] = return()
atribuiSalasDias numDias numSalas (li:lis) = do
        tarefa1file <- openFile "Tarefa1_p2.txt" AppendMode
        hPutStrLn tarefa1file ("Dia" ++ show numDias)
        hClose tarefa1file
        appendSalas numSalas (li:lis)
        atribuiSalasDias (numDias + 1) numSalas (findSalas numSalas (li:lis))


findSalas numSalas [] = []                                           
findSalas 0 string = string
findSalas numSalas (li:lis) = do
        findSalas (numSalas-1) lis

appendSalas numSalas [] = return()                                    
appendSalas 0 (li:lis) = return()
appendSalas numSalas (li:lis) = do
        ficheiro <- openFile "Tarefa1_p2.txt" AppendMode
        hPutStrLn ficheiro ("Sala" ++ show numSalas ++ "->" ++ unwords(tail(tail(words li))))
        hClose ficheiro
        appendSalas (numSalas-1) lis


--tarefa2
tarefa2_p2 :: IO()
tarefa2_p2 = do 
    conteudoDisciplina <- readFile "ucs.txt"

    ficheiro <- openFile "Tarefa2.txt" WriteMode
    hClose ficheiro
    
    ficheiro <- openFile "backup2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "backup3.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "backup.txt" WriteMode
    hPutStrLn ficheiro "0"
    hClose ficheiro

    copyFile (lines conteudoDisciplina)

    backup2 <- readFile' "backup2.txt"
    maiorAno (lines backup2)
    let backup = readFile' "backup.txt"
    maiorAnoString <- backup
    let maiorAnoInt = read maiorAnoString :: Int 
    

    putStrLn "Quantos dias podem ocorrer os exames?"
    dias <- getLine
    putStrLn "Quantas salas disponiveis por dia?"
    salas <- getLine
    
    let n_disciplinas = length (lines conteudoDisciplina) 
    
    let numDias = read dias :: Int
    let numSalas = read salas :: Int

    filtro2 maiorAnoInt 1 numDias numSalas (lines conteudoDisciplina)

    backup2 <- readFile' "backup2.txt"

    if null (lines backup2)
        then return()
        else do 
            putStrLn "Tempo insuficiente para acomodar todos os exames"
            return()


maiorAno :: [String] -> IO()
maiorAno [] = return()
maiorAno (x:xs) = do 

    let backup = readFile' "backup.txt"
    
    backupString <-  backup
    let backupInt = read backupString :: Int

    let numero = head(tail(words x))
    let numeroInt = read numero :: Int

    if numeroInt > backupInt
        then do 
            ficheiro <- openFile "backup.txt" WriteMode
            hPrint ficheiro numeroInt
            hClose ficheiro
            maiorAno xs
        else maiorAno xs

filtro2 :: Int -> Int -> Int -> Int -> [String] -> IO()  
filtro2 anoMax dias diasMax salas [] = return() 
filtro2 anoMax dias diasMax salas (x:xs) = do

    if dias > diasMax
        then return()
        else do 

            ficheiro <- openFile "Tarefa2.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro

            ficheiro <- openFile "backup3.txt" WriteMode
            hClose ficheiro 

            repeater2 anoMax salas
    
            backup3 <- readFile' "backup3.txt"
            printSalas2 salas (lines backup3)

            backup2 <- readFile' "backup2.txt"
            if null (lines backup2)
                then return()
                else do
                        ficheiro <- openFile "backup.txt" WriteMode
                        hPutStrLn ficheiro "0"
                        hClose ficheiro
                        maiorAno (lines backup2)
                        let backup = readFile' "backup.txt"
                        maiorAnoString <- backup
                        let maiorAnoInt = read maiorAnoString :: Int 
                        filtro2 maiorAnoInt (dias+1) diasMax salas (lines backup2)

finder :: Int -> [String] -> IO()
finder _ [] = return()
finder ano (x:xs) = do 
    let anoLinhaString = head(tail(words x))
    let anoLinhaInt = read anoLinhaString :: Int

    if ano == anoLinhaInt
        then do
            ficheiro <- openFile "backup3.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            copyFile xs
            return()
        else do
            ficheiro <- openFile "backup2.txt" AppendMode
            hPutStrLn ficheiro x
            hClose ficheiro 
            finder ano xs


repeater2 :: Int -> Int -> IO()
repeater2 0 _ = return()
repeater2 _ 0 = return()
repeater2 anoMax salas = do 
    backup2 <- readFile' "backup2.txt"

    ficheiro <- openFile "backup2.txt" WriteMode
    hClose ficheiro 

    finder anoMax (lines backup2)
    repeater2 (anoMax-1)(salas-1)

printSalas2 :: Int -> [String] -> IO()
printSalas2 salas [] = return()
printSalas2 0 (x:xs) = return()
printSalas2 salas (x:xs) = do
    ficheiro <- openFile "Tarefa2.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ unwords (tail (tail(words x))))
    hClose ficheiro
    printSalas2 (salas-1) xs

tarefa3_p2:: IO()
tarefa3_p2 = do 
    tarefa3_parte2
    ficheirobackup <- readFile "backup.txt"
    let tamanhoInicial = length (lines ficheirobackup) 
    let tamanhoFinal = length (remDup(lines ficheirobackup)) 
    print tamanhoFinal
    let incompativeis = tamanhoInicial - tamanhoFinal
    print incompativeis

remDup :: Eq a => [a] -> [a]
remDup [] = []
remDup (x:xs)
    | x `elem` xs = x : remDup (delete x xs)
    | otherwise = x : remDup xs

tarefa3_parte2 :: IO()
tarefa3_parte2 = do
    ficheiro <- openFile "backup.txt" WriteMode
    hClose ficheiro
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    putStrLn "indique o nome da disciplina"
    disciplina <- getLine
    encontrarNome disciplina (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
    putStrLn "indique o nome da outra disciplina"
    disciplina2 <- getLine
    encontrarNome disciplina2 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
    
encontrarNome:: String -> [String] -> [String] -> [String]-> IO() 
encontrarNome x [] y z = return()
encontrarNome input (linha:linhas) conteudo_insc conteudo_alunos = do
    let numero = head (words linha)
    if input == unwords (tail(tail(words linha)))
        then descobrirAlxxx numero conteudo_insc conteudo_alunos 
            else encontrarNome input linhas conteudo_insc conteudo_alunos

descobrirAlxxx:: String -> [String] -> [String]-> IO() 
descobrirAlxxx numero [] conteudo_alunos = return()
descobrirAlxxx numero (linha:linhas) conteudo_alunos = do
    let numero_al = head (words linha)
    if last (words linha) == numero
        then do 
            ficheiro <- openFile "backup.txt" AppendMode
            hPutStrLn ficheiro (head (words linha))
            hClose ficheiro
        else return ()
    descobrirAlxxx numero linhas conteudo_alunos

tarefa4_p2 :: IO()
tarefa4_p2 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"
    
    conteudoDisciplina <- readFile "ucs.txt"
    ficheiro <- openFile "Tarefa1_p2.txt" WriteMode
    hClose ficheiro

    putStrLn "Quantos dias podem ocorrer os exames?"
    dias <- getLine
    putStrLn "Quantas salas disponiveis por dia?"
    salas <- getLine
    
    let n_disciplinas = length (lines conteudoDisciplina) 
    
    let numDias = read dias :: Int
    let numSalas = read salas :: Int
    if n_disciplinas > numDias * numSalas  
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else filtro14 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) 1 numSalas (lines conteudoDisciplina)

filtro14 :: [String]-> [String]-> [String]->Int -> Int -> [String] -> IO()  
filtro14 disciplina inscricao alunos dias salas [] = return() 
filtro14 disciplina inscricao alunos dias salas (x:xs) = do

    ficheiro <- openFile "backup.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro

    printSalas4 disciplina inscricao alunos salas (x:xs)

    ficheirobackup <- readFile "backup.txt"
    let tamanhoInicial = length (lines ficheirobackup) 
    let tamanhoFinal = length (remDup(lines ficheirobackup)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Numero de imcompatibilidades: " ++ show incompativeis)
    hClose ficheiro
    
    filtro14 disciplina inscricao alunos (dias+1) salas (funcTail salas (x:xs))

printSalas4 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
printSalas4 disciplina inscricao alunos salas [] = return()
printSalas4 disciplina inscricao alunos 0 (x:xs) = return()
printSalas4 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "Tarefa1.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    printSalas4 disciplina inscricao alunos (salas-1) xs


    
--tarefa5
tarefa5_p2 :: IO()
tarefa5_p2 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"

    ficheiro <- openFile "Tarefa5.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "backup2.txt" WriteMode
    hClose ficheiro

    ficheiro <- openFile "backup3.txt" WriteMode
    hClose ficheiro


    copyFile(lines conteudoDisciplina) 

    putStrLn "Em quantos dias podem ocorrer os exames"
    dias <- getLine
    putStrLn "Quantas salas estao disponiveis"
    salas <- getLine

    let n_disciplinas = length (lines conteudoDisciplina)
    
    let numDias = read dias :: Int
    let numSalas = read salas :: Int

    if n_disciplinas > numDias * numSalas  
        then putStrLn "Dias insuficientes para acomodar todos os exames"
        else filtro15 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) 1 numSalas 

copyFile :: [String] -> IO()
copyFile [] = return()
copyFile (x:xs)= do 
    ficheiro <- openFile "backup2.txt" AppendMode
    hPutStrLn ficheiro x
    hClose ficheiro
    copyFile xs

filtro15 :: [String]-> [String]-> [String]->Int -> Int  -> IO()
filtro15 disciplina inscricao alunos dias salas  = do

    
    ficheiro <- openFile "backup.txt" WriteMode
    hClose ficheiro

    
    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
    hClose ficheiro

    ficheiro <- openFile "backup3.txt" WriteMode
    hClose ficheiro
    
    repeater salas salas 

    backup3 <- readFile' "backup3.txt"

    ficheiro <- openFile "backup.txt" WriteMode
    hClose ficheiro

    printSalas5 disciplina inscricao alunos salas (lines backup3) 

    ficheirobackup <- readFile "backup.txt"
    let tamanhoInicial = length (lines ficheirobackup) 
    let tamanhoFinal = length (remDup(lines ficheirobackup)) 
    let incompativeis = tamanhoInicial - tamanhoFinal

    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Numero de incompatibilidades: " ++ show incompativeis) 
    hClose ficheiro

    backup2 <- readFile' "backup2.txt"
    
    if null (lines backup2)
        then return()
        else filtro15 disciplina inscricao alunos (dias+1) salas

    
condTester :: Int -> Int-> [String] -> IO() 
condTester salas _ [] = return()
condTester 0 _ (x:xs) = return()
condTester salas salasTotal (x:xs) = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"

    if salas == salasTotal
        then do 
            backup3 <- openFile "backup3.txt" AppendMode
            hPutStrLn backup3 x 
            hClose backup3

            backup2 <- openFile "backup2.txt" WriteMode
            hClose backup2

            copyFile xs 
        else do 
            ficheiro <- openFile "backup.txt" WriteMode
            hClose ficheiro 
            backup3 <- readFile' "backup3.txt"
            let size = length (lines backup3)
            loaderSup2 (lines backup3) size 
            
            let disciplinas = unwords(tail(tail(words x)))
            encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
            
            ficheirobackup <- readFile' "backup.txt"
            let tamanhoInicial = length (lines ficheirobackup) 
            let tamanhoFinal = length (remDup(lines ficheirobackup)) 
            let incompativeisx = tamanhoInicial - tamanhoFinal

            if null xs 
                then do 
                    backup3 <- openFile "backup3.txt" AppendMode
                    hPutStrLn backup3 x 
                    hClose backup3

                    backup2 <- openFile "backup2.txt" WriteMode
                    hClose backup2
                    return()
                else do
                    
                    ficheiro <- openFile "backup.txt" WriteMode
                    hClose ficheiro 
                    backup3 <- readFile' "backup3.txt"
                    let size = length (lines backup3)
                    loaderSup2 (lines backup3) size 
                
                    let disciplinas = unwords(tail(tail(words (head xs))))
                    encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)
                    
                    ficheirobackup <- readFile' "backup.txt"
                    let tamanhoInicial = length (lines ficheirobackup) 
                    let tamanhoFinal = length (remDup(lines ficheirobackup)) 
                    let incompativeisxs = tamanhoInicial - tamanhoFinal

                    if incompativeisxs >= incompativeisx
                        then do 
                            backup3 <- openFile "backup3.txt" AppendMode
                            hPutStrLn backup3 x 
                            hClose backup3

                            backup2 <- openFile "backup2.txt" WriteMode
                            hClose backup2

                            copyFile xs 

                        else do 
                            backup3 <- openFile "backup3.txt" AppendMode
                            hPutStrLn backup3 (head xs )
                            hClose backup3

                            backup2 <- openFile "backup2.txt" WriteMode
                            hPutStrLn backup2 x
                            hClose backup2
                            
                            copyFile (tail xs) 

repeater :: Int -> Int-> IO()
repeater 0 salasTotal = return()
repeater salas salasTotal = do 

    backup2 <- readFile' "backup2.txt"
    condTester salas salasTotal (lines backup2)

    repeater (salas-1) salasTotal


loaderSup2 :: [String] -> Int -> IO()
loaderSup2 [] size = return()
loaderSup2 (x:xs) 0 = return()
loaderSup2 (x:xs) size = do 

    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"


    let disciplinas = unwords(tail(tail(words x)))
    encontrarNome disciplinas (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos)

    loaderSup2 xs (size-1)


printSalas5 :: [String]-> [String]-> [String]-> Int -> [String] -> IO()
printSalas5 disciplina inscricao alunos salas [] = return()
printSalas5 disciplina inscricao alunos 0 (x:xs) = return()
printSalas5 disciplina inscricao alunos salas (x:xs) = do

    let disciplinas = unwords (tail (tail(words x)))

    ficheiro <- openFile "Tarefa5.txt" AppendMode
    hPutStrLn ficheiro ("Sala "++ show salas ++ ": "++ disciplinas)
    hClose ficheiro

    encontrarNome disciplinas disciplina inscricao alunos
    printSalas5 disciplina inscricao alunos (salas-1) xs

--tarefa6
tarefa6_p2 :: IO()
tarefa6_p2 = do 
    conteudoDisciplina <- readFile "ucs.txt"
    conteudoInscricao <- readFile "inscricoes.txt"
    conteudoAlunos <- readFile "listaalunos.txt"


    ficheiro <- openFile "Tarefa6.txt" WriteMode
    hClose ficheiro

    ficheiro2 <- openFile "backup2.txt" WriteMode
    hClose ficheiro2 

    formatacao6 (lines conteudoDisciplina) (lines conteudoInscricao) (lines conteudoAlunos) (lines conteudoDisciplina)
    
    putStrLn "indique numero de dias em que o exame pode ocorrer"
    dias <- getLine
    putStrLn "indique o numero de salas disponiveis por dia"
    salas <- getLine
    putStrLn "indique a capacidade das salas no formato [Sala 1,Sala 2,...]"
    lotacao <- getLine
    let lotacaoInt = read lotacao :: [Int]

    let numDias = read dias :: Int
    let numSalas = read salas :: Int
    let lotacaoReverse = reverse lotacaoInt
    
    if length lotacaoReverse /= numSalas
    then do 
        putStrLn "lotacao diferente do numero de salas"
        return ()
    else do
            conteudobackup2 <- readFile "backup2.txt"
            ficheiro <- openFile "backup.txt" WriteMode
            hPutStrLn ficheiro (head (lines conteudobackup2))
            hClose ficheiro
            filtro16 1 numDias numSalas lotacaoReverse (lines conteudobackup2)

            backup2 <- readFile' "backup2.txt"
            if backup2 /= []
                then putStrLn ("Insuficiente para acomodar todos os exames")
                else return()

formatacao6 :: [String]->[String]->[String]->[String]->IO()
formatacao6 disciplina inscricao alunos [] = return()
formatacao6 disciplina inscricao alunos (x:xs) = do
    

    let disciplinas = unwords(tail(tail(words x)))

    ficheiro2 <- openFile "backup.txt" WriteMode
    hClose ficheiro2 

    encontrarNome disciplinas disciplina inscricao alunos

    backup1 <- readFile "backup.txt"
    let alunosInscritos = length (lines backup1)

    backup2 <- openFile "backup2.txt" AppendMode
    hPutStrLn backup2 (show alunosInscritos++" "++disciplinas)
    hClose backup2

    formatacao6 disciplina inscricao alunos xs
filtro16 :: Int -> Int -> Int -> [Int] -> [String] -> IO()
filtro16 dias diasMax salas lotacao [] = return() 
filtro16 dias diasMax salas lotacao (x:xs) = do
    if dias > diasMax 
        then return()
        else do
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("--- dia "++ show dias ++ "---")
            hClose ficheiro
            printSalas6 salas lotacao (x:xs) 
            conteudobackup2 <- readFile' "backup2.txt"
            filtro16 (dias+1) diasMax salas lotacao (lines conteudobackup2)

printSalas6 :: Int -> [Int] -> [String] -> IO()
printSalas6 salas (y:ys) [] = return()
printSalas6 0 (y:ys) (x:xs) = return()
printSalas6 salas [] [] = return()
printSalas6 salas [] (x:xs) = return()
printSalas6 salas (y:ys) (x:xs) = do
    
    backup <- readFile' "backup.txt" 

    let disciplina = unwords (tail(words x))


    
    let alunosTurma = head(words backup)
    let alunosTurmaint = read alunosTurma :: Int
    let alunosRestantes = alunosTurmaint - y 
    if alunosTurmaint > y
        then do 
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Sala "++ show salas ++ " "++ show y ++"/"++show y ++": "++ disciplina)
            hClose ficheiro
        else do 
            ficheiro <- openFile "Tarefa6.txt" AppendMode
            hPutStrLn ficheiro ("Sala "++ show salas ++ " "++ show alunosTurmaint ++"/"++show y ++": "++ disciplina)
            hClose ficheiro
    
    if alunosRestantes > 0 
        then do 
            ficheirobackup <- openFile "backup.txt" WriteMode
            hPutStrLn ficheirobackup (show alunosRestantes ++ " " ++ disciplina)
            hClose ficheirobackup
            printSalas6 (salas-1) ys (x:xs)
            return()
        else do 
            if null xs 
                then do 
                ficheiro2 <- openFile "backup2.txt" WriteMode 
                hClose ficheiro2 
                escreverFicheiro xs
                printSalas6 (salas-1) ys xs
                return()    
                else do 
                ficheirobackup <- openFile "backup.txt" WriteMode
                hPutStrLn ficheirobackup (head xs)  
                hClose ficheirobackup
                ficheiro2 <- openFile "backup2.txt" WriteMode 
                hClose ficheiro2 
                escreverFicheiro xs
                printSalas6 (salas-1) ys xs
                return()    

escreverFicheiro :: [String] -> IO()
escreverFicheiro [] = return()
escreverFicheiro (x:xs)= do 
    ficheiro2 <- openFile "backup2.txt" AppendMode
    hPutStrLn ficheiro2 x
    hClose ficheiro2 
    escreverFicheiro xs

funcTail :: Int -> [String] -> [String]
funcTail salas [] = []
funcTail 0 string = string
funcTail salas (x:xs) = do
    funcTail (salas-1) xs
