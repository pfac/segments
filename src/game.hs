module Main where

import IO				-- Input/Output, permite a escrita de valores no ecra e a leitura do teclado
import System.Random	-- gerar números aleatórios
import Data.Char		-- manipular caracteres e strings
import Data.Time		-- data e hora
import Directory		-- directorios, permite a manipulação de pastas no sistema

type Jogador = Bool	-- True é o jogador1, False é o jogador2
type Coluna = [Jogador]
type Conteudo = [Coluna]
-- Tabuleiro: TAB (Linhas,Colunas) Conteudo
data Tabuleiro = TAB (Int,Int) Conteudo
--	deriving (Show,Read,Eq)
instance Show Tabuleiro where
	-- show::Tabuleiro->String
	show a = desTab a
	
-- Jogada: JOG Peca Coluna
data Jogada = JOG Jogador Int
--	deriving (Show,Read)
type Turno = Int

matriz::Conteudo -- Experiência: exemplo de matriz 1
matriz=[[False,True],[True,True],[False,False,False,True,False,False,False,True,True,True],[False],[],[False,False],[],[True],[True,False,True],[True,True]]
matriz2::Conteudo
matriz2=[[False,False,True],[True],[],[True,True,False]]
tab1::Tabuleiro
tab1=TAB (3,4) matriz2

--------------------------------------------------------------------------------------------
-- valida uma dimensão, para que o valor introduzido pelo utilizador seja um valor entre 1 e 99 inclusive
valDim::Int->Bool
valDim d = (d>0)&&(d<100)

-- valida a jogada, devolvendo True se esta for válida, False se não for
valJog::Tabuleiro->Jogada->Bool
valJog (TAB (l,c) ct) (JOG _ k) = (k<=c)&&(colDisp (TAB (l,c) ct) k)

-- recebe um tabuleiro e o número da coluna a testar e devolve True se a coluna não tiver cheia, False se tiver cheia
colDisp::Tabuleiro->Int->Bool
colDisp (TAB (l,_) (e:es)) 1 = (length e)<l
colDisp (TAB (l,c) (e:es)) i = colDisp (TAB (l,c) es) (i-1)

-- Verifica se o tabuleiro está cheio
tabCheio::Tabuleiro->Bool
tabCheio (TAB (l,1) ct) = not (colDisp (TAB (l,1) ct) 1)
tabCheio (TAB (l,c) ct) = (not (colDisp (TAB (l,c) ct) c))&&(tabCheio (TAB (l,c-1) ct))

--------------------------------------------------------------------------------------------
-- PONTUAÇÃO
-- Calcula a pontuação actual do tabuleiro para um determinado jogador
pntTotal::Bool->Tabuleiro->Int
pntTotal _ (TAB (1,1) _) = 0
pntTotal j t = (pntsD1 j t)+(pntsD2 j t)+(pontosColuna j t)+(pontosLinha j t)

-- Transforma um Tabuleiro com comprimento e largura quaisquer num tabuleiro quadrado de ordem igual à maior das duas dimensões
quadTab::Tabuleiro->Tabuleiro
quadTab (TAB (l,c) ct) = if l>c then TAB (l,l) (ct++[[]]) else TAB (c,c) ct

-- Calcula a pontuação de uma lista de segmentos
pontos :: [Int] -> Int
pontos s = sum(map pontosAux s)

-- Associa o número de peças de um segmento à pontuação correspondente
pontosAux :: Int -> Int
pontosAux n = if (n==0) then 0 else (n-1) + pontosAux (n-1)

-- ///////////////////////
-- DIAGONAIS /
-- Forma o novo tabuleiro juntando a as diagonais à esquerda da diagonal principal do tabuleiro, esta e as diagonais à direita (diagonais /)
diag1::Bool->Tabuleiro->Tabuleiro
diag1 j (TAB (l,c) ct) = 	let	t = (quadTab (TAB (l,c) ct))
							;	(TAB (n,_) _) = t
							;	(TAB (_,_) ctEsq) = d1Esq j (remPrimLin (remUltCol t))
							;	(TAB (_,_) ctCnt) = d1Cnt j t
							;	(TAB (_,_) ctDir) = d1Dir j (remUltLin (remPrimCol t))
							in	TAB (n,(2*n)-1) (ctEsq++ctCnt++ctDir)

-- Devolve um Tabuleiro cujo conteudo é apenas uma coluna com a diagonal do tabuleiro original
d1Cnt::Bool->Tabuleiro->Tabuleiro
d1Cnt j (TAB (1,1) ct) = TAB (1,1) [[(toBool (ft (elemXinY 1 1 ct)) j)]]
d1Cnt j (TAB (l,c) ct) =	let	(TAB (_,_) (e:es)) = d1Cnt j (remPrimLin (remPrimCol (TAB (l,c) ct)))
							in TAB (l,1) (((toBool (ft (elemXinY 1 1 ct)) j):e):es)

-- Devolve um Tabuleiro cujas colunas representam as diagonais à esquerda da diagonal / do tabuleiro inicial
d1Esq::Bool->Tabuleiro->Tabuleiro
d1Esq j (TAB (1,1) ct) = d1Cnt j (TAB (1,1) ct)
d1Esq j (TAB (l,c) ct) = 	let	(TAB (_,_) nt) = d1Esq j (remPrimLin (remUltCol (TAB (l,c) ct)))
							;	(TAB (_,_) st) = d1Cnt j (TAB (l,c) ct)
							in	(TAB (l,c) (nt++st))

-- Devolve um Tabuleiro cujas colunas representam as diagonais à direita da diagonal / do tabuleiro inicial
d1Dir::Bool->Tabuleiro->Tabuleiro
d1Dir j (TAB (1,1) ct) = d1Cnt j (TAB (1,1) ct)
d1Dir j (TAB (l,c) ct) =	let (TAB (_,_) nt) = d1Dir j (remUltLin (remPrimCol (TAB (l,c) ct)))
							;	(TAB (_,_) st) = d1Cnt j (TAB (l,c) ct)
							in	(TAB (l,c) (st++nt))

-- Calcula a pontuação das diagonais / de um tabuleiro
pntsD1::Bool->Tabuleiro->Int
pntsD1 j t = pontosColuna j (diag1 j t)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
--DIAGONAIS \
-- Forma o novo tabuleiro juntando a as diagonais à esquerda da diagonal principal do tabuleiro, esta e as diagonais à direita (diagonais \)
diag2::Bool->Tabuleiro->Tabuleiro
diag2 j (TAB (l,c) ct) = 	let	t = (quadTab (TAB (l,c) ct))
							;	(TAB (n,_) _) = t
							;	(TAB (_,_) ctEsq) = d2Esq j (remUltLin (remUltCol t))
							;	(TAB (_,_) ctCnt) = d2Cnt j t
							;	(TAB (_,_) ctDir) = d2Dir j (remPrimLin (remPrimCol t))
							in	TAB (n,(2*n)-1) (ctEsq++ctCnt++ctDir)

-- Devolve um Tabuleiro cujo conteudo é apenas uma coluna com a diagonal \ do tabuleiro original
d2Cnt::Bool->Tabuleiro->Tabuleiro
d2Cnt j (TAB (1,1) ct) = TAB (1,1) [[(toBool (ft (elemXinY 1 1 ct)) j)]]
d2Cnt j (TAB (l,c) ct) =	let	(TAB (_,_) (e:es)) = d2Cnt j (remPrimLin (remUltCol (TAB (l,c) ct)))
							in TAB (l,1) (((toBool (ft (elemXinY 1 c ct)) j):e):es)

-- Devolve um Tabuleiro cujas colunas representam as diagonais à esquerda da diagonal \ do tabuleiro inicial
d2Esq::Bool->Tabuleiro->Tabuleiro
d2Esq j (TAB (1,1) ct) = d2Cnt j (TAB (1,1) ct)
d2Esq j (TAB (l,c) ct) = 	let	(TAB (_,_) nt) = d2Esq j (remUltLin (remUltCol (TAB (l,c) ct)))
							;	(TAB (_,_) st) = d2Cnt j (TAB (l,c) ct)
							in	(TAB (l,c) (nt++st))

-- Devolve um Tabuleiro cujas colunas representam as diagonais à esquerda da diagonal \ do tabuleiro inicial
d2Dir::Bool->Tabuleiro->Tabuleiro
d2Dir j (TAB (1,1) ct) = d2Cnt j (TAB (1,1) ct)
d2Dir j (TAB (l,c) ct) =	let (TAB (_,_) nt) = d2Dir j (remPrimLin (remPrimCol (TAB (l,c) ct)))
							;	(TAB (_,_) st) = d2Cnt j (TAB (l,c) ct)
							in	(TAB (l,c) (st++nt))

-- Calcula a pontuação das diagonais \ de um tabuleiro
pntsD2::Bool->Tabuleiro->Int
pntsD2 j t = pontosColuna j (diag2 j t)

--_____________________________
--LINHAS
-- Constrói uma lista com o número de peças de cada segmento de uma linha
segLin::Bool->Int->Conteudo->Int->[Int]
segLin _ _ [] t = [t]
segLin j i (c:cs) t = if (j==(toBool (ft(elemXinY i 1 (c:cs))) j))
							 then (segLin j i cs (t+1))
							 else (t:(segLin j i cs 0))

-- Faz a soma dos pontos de todas as linhas
pontosLinha::Bool->Tabuleiro->Int
pontosLinha j (TAB (l,c) ct) | l==1 = pontos(segLin j l ct 0)
                             | otherwise = pontos(segLin j l ct 0) + (pontosLinha j (TAB (l-1,c) ct))

-- |||||||||||||||||||||||||||||||||
-- COLUNAS
-- Constrói uma lista com o número de peças de cada segmento de uma coluna
segCol::Bool->Coluna->Int->[Int]
segCol _ [] t = [t]
segCol j (e:es) t = if (j==e)
						then (segCol j es (t+1))
						else t:(segCol j es 0)

--Faz a soma dos pontos de todas as colunas
pontosColuna::Bool->Tabuleiro->Int
pontosColuna _ (TAB (_,_) []) = 0
pontosColuna j (TAB (l,c) (m:ms)) = pontos(segCol j m 0) + (pontosColuna j (TAB (l,c-1) ms))

--------------------------------------------------------------------------------------------
-- realiza uma jogada
jogar::Tabuleiro->Jogada->Conteudo->Tabuleiro
jogar (TAB (l,c) (e:es)) (JOG j 1) ct = (TAB (l,c) (ct++((e++[j]):es)))
jogar (TAB (l,c) (e:es)) (JOG j k) ct = jogar (TAB (l,c) es) (JOG j (k-1)) (ct++[e])

--------------------------------------------------------------------------------------------
-- desenha o tabuleiro no ecrã
desTab::Tabuleiro->String
desTab (TAB (l,c) ct) =	let	{	nr	= if l<10 then " 0"++(show l) else " "++(show l)
							;	line = nr++"|"++(desLinha (TAB (l,c) ct))++"|\n"
							}
						in	if l==1
							then line++(desCoord (TAB (l,c) ct))
							else line++(desTab (TAB (l-1,c) ct))

-- desenha a última linha do tabuleiro (sendo a última linha a definida na dimensão do mesmo)
desLinha::Tabuleiro->String
desLinha (TAB (l,1) ct) = peca (elemXinY l 1 ct)
desLinha (TAB (l,c) ct) = (desLinha (TAB (l,c-1) ct))++"|"++peca (elemXinY l c ct)

-- associa a cada bool a peca do jogador correspondente (e um espaço vazio na ausência de peça)
peca::Maybe Bool->String
peca (Just True) = "OO"
peca (Just False) = "XX"
peca Nothing = "  "

-- desenha a linha das coordenadas horizontais
desCoord::Tabuleiro->String
desCoord (TAB (_,1) ct) = "    01 "
desCoord (TAB (l,c) ct) | c<10 = (desCoord (TAB (l,c-1) ct))++"0"++(show c)++" "
						| otherwise = (desCoord (TAB (l,c-1) ct))++" "++(show c)

----------------------------------------------------------------------------------------------
-- outras funções
-- devolve o elemento da linha X na coluna Y
elemXinY::Int->Int->Conteudo->Maybe Bool
elemXinY _ _ [] = Nothing
elemXinY 1 1 ((e:es):cs) = Just e
elemXinY x 1 ((e:es):cs) = elemXinY (x-1) 1 (es:cs)
elemXinY x y (c:cs) = elemXinY x (y-1) cs

-- devolve o x-ésimo elemento de uma lista
elemX::[a]->Int->Maybe a
elemX [] _ = Nothing
elemX (e:es) 1 = Just e
elemX (e:es) x = elemX es (x-1)

-- texto automático do menu Ajuda e Informações
autoTexto::Int->IO ()
autoTexto op = do	{ putStrLn (unlines (mensagem op))
					; getLine
					; opAjudaInfo
					}

-- gera um número inteiro aleatório entre 1 e 2 inclusive
rnd::IO Int
rnd = getStdRandom (randomR (1,2))

-- utilizada na interface gráfica. Associa a cada bool o número do jogador correspondente
bool2Str::Bool->String
bool2Str True = "1"
bool2Str False = "2"

-- Para preenchimento dos espaços vazios quando constrói um tabuleiro para calcular os segmentos de cada jogador.
-- Recebe uma lista (para utilização nas funções) com um só bool, e outro bool (Jogador). Se for um espaço vazio (uma lista vazia), então
-- será associado a esse espaço o valor contrário ao jogador para o qual se está a calcular a pontuação
toBool::[Bool]->Bool-> Bool
toBool [] True = False
toBool [] False = True
toBool [x] j = x==True

-- False True. Converte um valor Maybe Bool para uma lista, vazia, ou apenas com o valor correspondente.
ft::Maybe Bool->[Bool]
ft x = if x==Nothing 
		then []
		else if x==Just True
				then [True]
				else [False]

-- Remove o primeiro elemento da lista
myTail::[a]->[a]
myTail [] = []
myTail (e:es) = es

-- Remove o último elemento da lista
myNeck::[a]->[a]
myNeck [] = []
myNeck [x] = []
myNeck (x:xs) = x:(myNeck xs)

-- Remove a primeira coluna
remPrimCol::Tabuleiro->Tabuleiro
remPrimCol (TAB (l,c) ct) = TAB (l,c-1) (myTail ct)

-- Remove a última coluna
remUltCol::Tabuleiro->Tabuleiro
remUltCol (TAB  (l,c) ct) = TAB (l,c-1) (myNeck ct)

-- Remove a primeira linha
remPrimLin::Tabuleiro->Tabuleiro
remPrimLin (TAB (l,c) ct) = TAB (l-1,c) (map myTail ct)

-- Remove a última linha
remUltLin::Tabuleiro->Tabuleiro
remUltLin (TAB (l,c) ct) = TAB (l-1,c) (remLinX l ct)

-- Remove a linha X de um conteudo
remLinX::Int->Conteudo->Conteudo
remLinX _ [] = []
remLinX x (c:cs) = if (length c)>=x
					then (myNeck c):(remLinX x cs)
					else c:(remLinX x cs)

-- Verifica se uma string é somente definida por números
myNumber::String->Bool
myNumber n = foldr (&&) True (map isDigit n)

-- Remove o caracter da string
tiraChar::String->Char->String
tiraChar [] _ = []
tiraChar (s:ss) c = if s==c then tiraChar ss c else s:(tiraChar ss c)

-- devolve os primeiros caracteres da String até encontrar um determinado caracter
leftUntil::String->Char->String
leftUntil [] _ = []
leftUntil (s:ss) c = if s==c then [] else s:(leftUntil ss c)

-- Verifica se um ficheiro foi formado pelo jogo (se tem 12 caracteres ++ extensão ".seg")
isMyFile::String->Bool
isMyFile (_:_:_:_:_:_:_:_:_:_:_:_:".seg") = True
isMyFile _ = False

-- transforma o caracter inserido no ficheiro para o bool correspondente
char2Bool::Char->Bool
char2Bool '1' = True
char2Bool '2' = False

-- devolve a cauda de uma lista a partir da posição x
tailFromX::[a]->Int->[a]
tailFromX [] _ = []
tailFromX (e:es) 1 = es
tailFromX (e:es) x = tailFromX es (x-1)

-- recebe o nome de um ficheiro e constrói um par com duas String, a hora e data
formatHoraData::String->(String,String)
formatHoraData ('S':'e':'g':'m':'e':'n':'t':'o':'s':'\\':h1:h2:min1:min2:y1:y2:y3:y4:m1:m2:d1:d2:_) = 	let	hour = [h1,h2,':',min1,min2]
															;	date = [d1,d2,'-',m1,m2,'-',y1,y2,y3,y4]
															in	(hour,date)

----------------------------------------------------------------------------------------------
-- Interface gráfica
-- menu inicial
main::IO ()
main = do 	{ putStr (unlines (mensagem 0))
			; mnuMain
			}

mnuMain::IO ()
mnuMain = do	{ op <- getLine
				; case op of
					'1':_ -> opIniciarJogo
					'2':_ -> opCarregarJogo
					'3':_ -> opRegisto
					'4':_ -> opAjudaInfo
					'0':_ -> do	{ putStrLn (unlines [""," Funcao encerrada..."," Escreva 'main' para iniciar o jogo."])
								; return ()
								}
					otherwise -> opcaoInv 0
				}

-- Iniciar Jogo
opIniciarJogo::IO ()
opIniciarJogo = do	{ putStrLn (unlines (mensagem 1))
					; mnuIniciarJogo
					}

mnuIniciarJogo = do { op <- getLine
					; case op of
						'1':_ -> op1Jog
						'2':_ -> op2Jog
						'0':_ -> main
						otherwise -> opcaoInv 1
					}

--		Um jogador
op1Jog::IO()
op1Jog = do	{ j <- rnd
			; d <- mnuDific
			; t <- getTab
			; f <- genFP--	Gera o filePath
			; putStrLn ""
			; putStrLn (show t)
			; putStrLn ""
			; putStrLn " Iniciar jogo..."
			; ciclo1Jog t 1 (j==1) f d
			}

ciclo1Jog::Tabuleiro->Turno->Jogador->String->Int->IO ()
ciclo1Jog t k j f d = if j
						then do { putStrLn (" "++(show k)++"o turno")
								; putStr "\n Em que coluna deseja jogar? "
								; c <- getLine
								; let col = (read c)::Int
								; let nt = jogar t (JOG True col) []
								; if (valJog t (JOG True col))
									then do { putStrLn ""
											; putStrLn (show nt)
											; let pts1 = pntTotal True nt
											; let pts2 = pntTotal False nt
											; putStrLn "\n Pontuacao actual:"
											; putStr " Jogador 1: "
											; putStrLn (show pts1)
											; putStr " Jogador 2: "
											; putStrLn (show pts2)
											; putStrLn ""
											; gravaSegFile f nt (not j) (k+1) 1 d
											; if tabCheio nt
												then fim1Jog nt f d
												else ciclo1Jog nt (k+1) False f d
											}
									else do	{ putStrLn " Jogada invalida."
											; ciclo1Jog t k j f d
											}
								}
						else do	{ putStr ""	
								; jogada <- case d of
												1 -> jogaCompFcl t
												2 -> jogaCompMed t
												3 -> jogaCompDif t
								; let nt = jogar t jogada []
								; putStrLn ""
								; putStrLn (show nt)
								; let pts1 = pntTotal True nt
								; let pts2 = pntTotal False nt
								; putStrLn "\n Pontuacao actual:"
								; putStr " Jogador 1: "
								; putStrLn (show pts1)
								; putStr " Jogador 2: "
								; putStrLn (show pts2)
								; putStrLn ""
								; if tabCheio nt
											then fim1Jog nt f d
											else ciclo1Jog nt (k+1) True f d
								}

fim1Jog::Tabuleiro->String->Int->IO ()
fim1Jog t f d = do	{ putStrLn " Fim do Jogo"
					; let pts1 = pntTotal True t
					; let pts2 = pntTotal False t
					; let r = if pts1>pts2 then 1 else (if pts2>pts1 then 2 else 0)
					; let p = if r==1 then pts1 else pts2
					; if r==0
						then do putStrLn " O jogo acaba empatado."
						else do	putStrLn (" Vence "++(if r==1 then "o jogador" else "o computador"))
					; putStrLn (" Pontuacao final: "++(show p))
					; let dif = case d of
									1 -> "Facil"
									2 -> "Intermedio"
									3 -> "Dificil"
					; putStrLn (" Dificuldade: "++dif)
					; putStrLn "\n pressione Enter para continuar\n"
					; isItThere <- doesFileExist f
					; if isItThere
						then do	{ putStrLn (" A remover ficheiro temporario "++f)
								; removeFile f
								; putStrLn " Concluido...\n"
								}
						else putStrLn " Ficheiro temporario nao existente."
					; if r==1
						then do	{ let TAB (l,c) _ = t in gravaHstFile (l,c) p (r==1)
								; getLine
								; main
								}
						else do	{ getLine
								; main
								}
					}


mnuDific::IO Int
mnuDific = do	{ putStr (unlines (mensagem 11))
				; d <- getDific
				; return d
				}

getDific::IO Int
getDific = do	{ op <- getLine
				; case op of
					'1':_ -> return 1
					'2':_ -> return 2
					'3':_ -> return 3
					'0':_ -> return 0
					otherwise -> do	{ putStrLn "\n Opcao Invalida"
									; getDific
									}
				}

jogaCompFcl::Tabuleiro->IO Jogada
jogaCompFcl t = do	{ rn <- rndPlay t
					; if valJog t (JOG False rn)
						then return (JOG False rn)
						else jogaCompFcl t
					}

rndPlay::Tabuleiro -> IO Int
rndPlay (TAB (l,c) ct) = getStdRandom (randomR (1,c))

jogaCompMed::Tabuleiro->IO Jogada
jogaCompMed t = do	{ r <- rnd
					; nj <- (if r==1 then jogaCompFcl t else jogaCompDif t)
					; return nj
					}

jogaCompDif::Tabuleiro->IO Jogada
jogaCompDif t = do	{ let nj = jogaCompDifAux t (0,0)
					  in return nj
					}

jogaCompDifAux::Tabuleiro->(Int,Int)->Jogada
jogaCompDifAux (TAB (l,1) ct) (n,p) = let pn = pntTotal False (jogar (TAB (l,1) ct) (JOG False 1) [])
								in if (colDisp (TAB (l,1) ct) 1)
									then if pn>=p
											then (JOG False 1)
											else (JOG False n)
									else (JOG False n)
jogaCompDifAux (TAB (l,c) ct) (n,p) = let pn = pntTotal False (jogar (TAB (l,c) ct) (JOG False c) [])
								in if (colDisp (TAB (l,c) ct) c)
									then if pn>=p
											then jogaCompDifAux (TAB (l,c-1) ct) (c,pn)
											else jogaCompDifAux (TAB (l,c-1) ct) (n,p)
									else jogaCompDifAux (TAB (l,c-1) ct) (n,p)


--		Dois Jogadores
op2Jog::IO ()
op2Jog = do	{ j <- rnd
			; t <- getTab
			; f <- genFP--	Gera o filePath
			; putStrLn ""
			; putStrLn " Iniciar jogo..."
			; ciclo2Jog t 1 (j==1) f
			}

-- Inicia o ciclo a dois jogadores. Recebe o tabuleiro actual, o turno seguinte, o próximo jogador e o nome do ficheiro temporário
ciclo2Jog::Tabuleiro->Turno->Jogador->String->IO ()
ciclo2Jog t k j f = do	{ putStrLn (" "++(show k)++"o turno - jogador "++(bool2Str j))
						; putStr " Em que coluna deseja jogar? "
						; c <- getLine
						; let nt = (jogar t (JOG j col) [])
						      col = ((read c)::Int)
						; if (valJog t (JOG j col))
							then do { putStrLn ""
									; putStrLn (show nt)
									; let pts1 = pntTotal True nt
									; let pts2 = pntTotal False nt
									; putStrLn "\n Pontuacao actual:"
									; putStr " Jogador 1: "
									; putStrLn (show pts1)
									; putStr " Jogador 2: "
									; putStrLn (show pts2)
									; putStrLn ""
									; gravaSegFile f nt (not j) (k+1) 2 0
									; if tabCheio nt
										then fim2Jog nt f
										else ciclo2Jog nt (k+1) (not j) f
									}
							else do	{ putStrLn " Jogada invalida."
									; ciclo2Jog t k j f
									}
						}

-- Finaliza o jogo (2jogadores). Recebe o tabuleiro final e o nome do ficheiro temporário (para apagar)
fim2Jog::Tabuleiro->String->IO ()
fim2Jog t f = do	{ putStrLn " Fim do Jogo"
					; let pts1 = pntTotal True t
					; let pts2 = pntTotal False t
					; let r = if pts1>pts2 then 1 else (if pts2>pts1 then 2 else 0)
					; let p = if r==1 then pts1 else pts2
					; if r==0
						then do putStrLn " O jogo acaba empatado."
						else do	putStrLn (" Vence o jogador "++(show r))
					; putStrLn (" Pontuacao final: "++(show p))
					; putStrLn "\n pressione Enter para continuar\n"
					; putStrLn (" A remover ficheiro temporario "++f)
					; removeFile f
					; putStrLn " Concluido...\n"
					; if r==0
						then do	{ getLine
								; main
								}
						else do	{ let TAB (l,c) _ = t in gravaHstFile (l,c) p (r==1)
								; getLine
								; main
								}
					}

-- Carregar Jogo
opCarregarJogo::IO ()
opCarregarJogo = do	{ putStrLn (unlines (mensagem 2))
					; isItThere <- doesDirectoryExist "Segmentos"
					; if isItThere
						then do	{ fileList <- getDirectoryContents "Segmentos"
								; let myFiles = (filter isMyFile fileList)
								; if (length myFiles)>0
									then do	{ escreveFich myFiles 1
											; nrF <- opEscFich myFiles
											; let Just nomeF = elemX myFiles nrF
											; inside <- readFile ("Segmentos\\"++nomeF)
											; let ct = map (map char2Bool) (tailFromX (lines inside) 6)
											; let Just auxL = (elemX (lines inside) 5)
											; let l = (read auxL)::Int
											; let Just auxC = (elemX (lines inside) 6)
											; let c = (read auxC)::Int
											; let t = TAB (l,c) ct
											; let Just auxTp = (elemX (lines inside) 1)
											; let tipo = (read auxTp)::Int
											; let Just auxDif = (elemX (lines inside) 2)
											; let d = (read auxDif)::Int
											; let Just trn = (elemX (lines inside) 3)
											; let turno = (read trn)::Int
											; let Just auxJ = (elemX (lines inside) 4)
											; let j = (read auxJ)::Bool
											; case tipo of
												1 -> ciclo1Jog t turno j ("Segmentos\\"++nomeF) d
												2 -> ciclo2Jog t turno j ("Segmentos\\"++nomeF)
											}
									else errCarregar
								}
						else errCarregar
					}

errCarregar::IO ()
errCarregar = do	{ putStrLn " Nao existem jogos guardados.\n\n pressione Enter para continuar\n"
					; getLine
					; main
					}

opEscFich::[String]->IO Int
opEscFich fs = do	{ putStr "\n Insira o numero do ficheiro que pretende carregar: "
					; op <- getLine
					; let nrF = (read op)::Int
					; if (nrF>0)&&(nrF<=(length fs))
						then return nrF
						else do	{ putStrLn "\n Opcao invalida. O numero escolhido nao corresponde a nenhum ficheiro"
								; opEscFich fs
								}
					}

-- Registo
opRegisto::IO ()
opRegisto = do	{ putStr (unlines (mensagem 3))
				; mnuRegisto
				}

mnuRegisto = do	{ op <- getLine
				; case op of
					'1':_ -> leHist
					'0':_ -> main
				}

-- lê um ficheiro de historico e escreve no ecra os registos encontrados
leHist::IO ()
leHist = do	{ isItThere <- doesFileExist "Segmentos\\historico.hst"
			; if isItThere
				then do	{ inside <- readFile "Segmentos\\historico.hst"
						; if (length inside)>0
							then do	{ putStrLn " Encontrados registos...\n"
									; let orgRcs = leHistAux (lines inside)
									; escHist orgRcs 1
									; getLine
									; main
									}
							else errHist
						}
				else errHist
			}

-- recebe uma lista de registos de um histórico e devolve uma lista com os mesmos registos devidamente organizados para serem utilizados
leHistAux::[String]->[(Int,Int,Int,String)]
leHistAux [] = []
leHistAux ((l1:l2:c1:c2:p1:p2:p3:p4:nome):rs) = let	lin = (read [l1,l2])::Int
												;	col = (read [c1,c2])::Int
												;	pts = (read [p1,p2,p3,p4])::Int
												in (lin,col,pts,nome):(leHistAux rs)

-- dada uma lista organizada de registos, escreve no ecrã os registos dessa lista
escHist::[(Int,Int,Int,String)]->Int->IO ()
escHist [] k = do	{ putStrLn ("\n Lidos "++(show (k-1))++" registos.\n pressione Enter para continuar\n")
					}
escHist ((l,c,p,nome):rs) k = do	{ putStrLn ("\t"++(show k)++".\tLinhas: "++(show l))
									; putStrLn ("\t\tColunas: "++(show c))
									; putStrLn ("\t\tVencedor: "++nome++", com "++(show p)++" pontos.\n")
									; escHist rs (k+1)
									}

-- mensagem de erro do histórico
errHist::IO ()
errHist = do	{ putStrLn "\n\tNao existem registos no historico.\n"
				; opRegisto;
				}

-- Ajuda e Informações
opAjudaInfo::IO ()
opAjudaInfo = do	{ putStrLn (unlines (mensagem 4))
					; mnuAjudaInfo
					}
mnuAjudaInfo::IO ()
mnuAjudaInfo = do	{ op <- getLine
					; case op of
						'1':_ -> autoTexto 41
						'2':_ -> autoTexto 42
						'5':_ -> autoTexto 45
						'0':_ -> main
						otherwise -> opcaoInv 3
					}

-- Grava o ficheiro temporário. Recebe o nome do ficheiro, o tabuleiro a gravar, o próximo jogador, o próximo turno, o tipo de jogo a gravar (1 -> 1jogador, 2->2jogadores) e a dificuldade (se não for um jogo do tipo 1 é 0)
gravaSegFile::String->Tabuleiro->Jogador->Turno->Int->Int->IO ()
gravaSegFile f (TAB (l,c) ct) j trn tj d = do	{ putStrLn " A gravar dados..."
												; let strCt = map (foldr (++) []) (map (map bool2Str) ct)
												; let strFinal = [(show tj),(show d),(show trn),(show j),(show l),(show c)]++strCt
												; writeFile f (unlines strFinal)
												; return ()
												}

-- Grava o ficheiro de histórico. Recebe as dimensões do Tabuleiro, a pontuação do vencedor, e o valor correspondente a este
gravaHstFile::(Int,Int)->Int->Jogador->IO ()
gravaHstFile (l,c) p j = do	{ putStrLn " A analisar historico..."
							; isHstThere <- doesFileExist "Segmentos\\historico.hst"
							; if isHstThere
								then do	{ putStrLn " Ficheiro encontrado"
										; inside <- verifHstFile
										; if (length inside)>9
											then gravaHstAux (l,c) p j (myNeck (lines inside))
											else gravaHstAux (l,c) p j (lines inside)
										}										
								else gravaHstAux (l,c) p j []
							}

verifHstFile::IO String
verifHstFile = do	{ inside <- readFile "Segmentos\\historico.hst"
					; return inside
					}

gravaHstAux::(Int,Int)->Int->Jogador->[String]->IO ()
gravaHstAux (l,c) p j ss = do	{ putStr ("\n Parabens jogador "++(bool2Str j)++". Insira o seu nome: ")
								; nome <- getLine
								; let strC = if (length (show c))==2 then show c else '0':(show c)
								; let strL = if (length (show l))==2 then show l else '0':(show l)
								; let strP = case length (show p) of
												1 -> '0':'0':'0':(show p)
												2 -> '0':'0':(show p)
												3 -> '0':(show p)
												4 -> (show p)
								; putStrLn " A guardar registo..."
								; writeFile "Segmentos\\historico.hst" (unlines ((strC++strL++strP++nome):ss))
								; putStrLn " Concluido..."
								}
----------------------------------------------------------------------------------------------
-- menus
-- devolve o texto de erro e repete a funcao que recebe a opcao
opcaoInv::Int->IO ()
opcaoInv op = do	{ putStrLn " Opcao invalida"
					; case op of
						0 -> mnuMain
						1 -> mnuIniciarJogo
						3 -> mnuAjudaInfo
					}

-- constroi o tabuleiro inicial
getTab::IO Tabuleiro
getTab = do	{ putStrLn " Defina a dimensao do tabuleiro"
			; l <- getDim True
			; c <- getDim False
			; return (TAB (l,c) (fazerCont c))
			}

-- imprime o texto correspondente e pede as dimensoes do tabuleiro
getDim::Bool->IO Int
getDim a = do 	{ if a
						then putStr " Numero de linhas:"
						else putStr " Numero de colunas:"
				; d <- getLine
				; let dim = if (myNumber d) then ((read d)::Int) else 0
				; if (valDim dim)&&(dim/=0)
					then (return dim)
					else do	{ putStrLn " Opcao invalida\n"
							; getDim a
							}
				}

-- gera o nome do ficheiro
genFP::IO String
genFP = do	{ putStrLn " A iniciar ficheiro temporario..."
			; now <- getCurrentTime
			; let strData = tiraChar (show (utctDay now)) '-'
			; let auxStrMin = leftUntil (show ((utctDayTime now)/60)) '.'
			; let intMin = (read auxStrMin)::Int
			; let (horas,minutos) = divMod intMin 60
			; let strHora = if (length (show horas))==2 then (show horas) else '0':(show horas)
			; let strMin = if (length (show minutos))==2 then (show minutos) else '0':(show minutos)
			; let id = strHora++strMin++strData
			; dir <- (doesDirectoryExist "Segmentos")
			; if dir
				then putStr ""
				else createDirectory  ("Segmentos")
			; return ("Segmentos\\"++id++".seg")
			}

-- Escreve no ecra a lista dos ficheiros (recebe a lista de ficheiros e 1 (acumulador para numerar as opções))
escreveFich::[String]->Int->IO ()
escreveFich [] _ = return ()
escreveFich (s:ss) i = do	{ putStr ""
							; let (h,d) = (formatHoraData ("Segmentos\\"++s))
							; putStrLn (" "++(show i)++".\t"++s)
							; putStrLn ("\t\tCriado em: "++d++" "++h)
							; cont <- readFile ("Segmentos\\"++s)
							; let contS = lines cont
							; let (_:_:_:lin:col:_) = contS
							; putStrLn ("\tLinhas: "++lin)
							; putStrLn ("\tColunas: "++col)
							; putStrLn ""
							; escreveFich ss (i+1)
							}

-- cria o conteudo vazio inserindo uma lista vazia para cada coluna
fazerCont::Int->Conteudo
fazerCont c = if c==1 then [[]] else []:(fazerCont (c-1))

-- textos utilizados no programa
mensagem::Int->[String]
mensagem op = case op of
				0 ->	["",
						"  _____   _____   _____   _     _   _____   _   _   _____   _____   _____",
						" |  ___| |  ___| |  ___| | \\   / | |  ___| | | | | |_   _| |  _  | |  ___|",
						" | |___  | |__   | | __  |  \\_/  | | |__   |  \\| |   | |   | | | | | |___",
						" |___  | |  __|  | ||_ | | .   . | |  __|  |     |   | |   | | | | |___  |",
						"  ___| | | |___  | |__|| | |\\_/| | | |___  | |\\  |   | |   | |_| |  ___| |",
						" |_____| |_____| |_____| |_|   |_| |_____| |_| |_|   |_|   |_____| |_____|",
						"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
						" Bem-vindo,",
						"",
						" 1 -> Iniciar jogo",
						" 2 -> Carregar jogo",
						" 3 -> Registo",
						" 4 -> Ajuda e Informacoes",
						"",
						" 0 -> Sair",
						"",
						" Escolha uma opcao: "]
				1 ->	[""
						," INICIAR JOGO+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
						,""
						," 1 -> 1 Jogador vs Computador"
						," 2 -> 2 Jogadores"
						,""
						," 0 -> Voltar ao menu anterior"
						,""
						," Escolha uma opcao: "]
				11 ->	[""
						," DIFICULDADE++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
						,""
						," 1 -> Facil"
						," 2 -> Intermedio"
						," 3 -> Dificil"
						,""
						," Escolha uma opcao: "]
				2 ->	["",
						" CARREGAR JOGO++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
						"",
						" Jogos guardados:",
						""]
				3 ->	[""
						," REGISTO++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
						,""
						," 1 -> Historico (ultimos 10 jogos)"
						,""
						," 0 -> Voltar ao menu anterior"
						,""
						," Escolha uma opcao: "]
				4 ->	["",
						" AJUDA E INFORMACOES++++++++++++++++++++++++++++++++++++++++++++++++++++++",
						" Escolha uma opcao",
						"",
						" >> INSTRUCOES >>",
						" 1 -> Regras",
						" 2 -> Pontuacao",
						"",
						" >> INFORMACOES >>",
						" 5 -> Autores",
						"",
						" 0 -> Voltar ao menu anterior",
						"",
						" Escolha uma opcao: "]
				41 ->	["",
						" REGRAS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
						" O objectivo do jogo e criar a maior sequencia possivel de marcas corres-",
						" pondentes ao jogador, sendo a pontuacao do jogador correspondente as se-",
						" quencias por ele ou ela criadas, proporcional a quantidade de pecas na",
						" sequencia.",
						" O jogo so acaba quando todo o tabuleiro tiver sido preenchido. Vence o",
						" jogador que obtiver maior pontuacao.",
						"",
						" pressione Enter para continuar",
						""]
				42 ->	["",
						" PONTUACAO++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
						" A pontuacao dum jogador depende das sequencias criadas por ele ou ela.",
						" Sao validas quaisquer sequencias horizontais, verticais ou diagonais. O",
						" valor de cada sequencia segue a seguinte tabela:",
						"\t ____________________  ____________________",
						"\t| Nr. pecas | Pontos || Nr. pecas | Pontos |",
						"\t|-----------|--------||-----------|--------|",
						"\t|  2 pecas  |    1   ||  7 pecas  |   21   |",
						"\t|  3 pecas  |    3   ||  8 pecas  |   28   |",
						"\t|  4 pecas  |    6   ||  9 pecas  |   36   |",
						"\t|  5 pecas  |   10   || 10 pecas  |   45   |",
						"\t|  6 pecas  |   15   || 11 pecas  |   55   |",
						"\t|___________|________||___________|________|",
						"",
						"\tA pontuacao segue a sucessao:",
						"\t\tp1=0",
						"\t\tpn=(n-1)+p(n-1)",
						"",
						"",
						" pressione Enter para continuar",
						""]
				45 ->	["",
						" Uma criacao",
						" ATUM - Alunos Talentosos da Universidade do Minho",
						" Dezembro de 2008",
						" Autores:",
						"\t\tAndre Teixeira.....54753",
						"\t\tFabio Morais.......54815",
						"\t\tPedro Costa........54736",
						"",
						"",
						" pressione Enter para continuar",
						""]