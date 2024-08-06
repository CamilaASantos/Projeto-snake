module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Direções: Movimentação da cobra
data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

-- Posicao: Utilizado para gerar a posição dos segmentos do corpo da cobra e comida (baseado em https://haskell.pesquisa.ufabc.edu.br/haskell/07.adt/ 2.1 - Tipo Soma)
type Pos = (Float, Float)

--Comida: Posicao em que está representacao gráfica da comida
type Food = (Pos, Picture)

--Snake: Representação da cobra no jogo
data Snake = Snake{
  corpo :: [Pos],  -- Armazena todos os "segmentos" que compoem o corpo da cobra
  snakeDir :: Dir, -- Armazena direcao atual da cobra
  speed :: Float, -- Velocidade de movimento
  state :: Bool, -- Armazena o estado da cobra ("viva" ou "morta")
  pontuacao :: Int --Armazena pontuacao
} deriving (Eq, Show)

--Game: Estados do jogo
data Game = Menu | Play Snake Food Bool | GameOver deriving (Eq, Show)

-- Funcao de movimentacao da cobra
movimento :: Snake -> Dir -> Snake
movimento  = undefined

--Extremidades da tela 
limiteTela :: Num a => a
limiteTela = 500

-- Checa se a cobra atingiu a extremidade do Tabuleiro
checaEx :: Snake -> Snake
checaEx cobra@(Snake ((x,y):_) _ _ estado _ ) =
  if x > limiteTela || x < -limiteTela || y > limiteTela || y < -limiteTela
  then cobra { state = False }
  else cobra

-- Checa se a cobra colidiu com seu corpo
checaColisao :: Snake -> Snake
checaColisao cobra@(Snake (x:xs) _ _ estado _ ) =
  if x `elem` xs
  then cobra { state = False }
  else cobra

-- Representação gráfica da fruta
desenhaComida :: Food -> Picture
desenhaComida ((x,y), img) = translate x y img

-- Função construída utilizando ChatGPT como ajuda
selecionaAleatorio :: [a] -> IO a
selecionaAleatorio [] = error "Lista de imagens não localizada"
selecionaAleatorio ls = do
  let tamanho = length ls
  indice <- randomRIO (0, tamanho - 1)
  return (ls !! indice)

-- Cria uma comida em uma posição aleatória
criaComida :: StdGen -> Picture -> Food 
criaComida g img = ((x, y), img)
  where
    (xGen, yGen) = split g
    (x, _) = randomR (-limiteTela, limiteTela) xGen
    (y, _) = randomR (-limiteTela, limiteTela) yGen

-- Função que desenha o menu
desenhaMenu :: Picture
desenhaMenu = pictures [
    translate (-100) 100 $ scale 0.5 0.5 $ color white $ text "Escolha a dificuldade:",
    translate (-100) 50 $ scale 0.3 0.3 $ color green $ text "Pressione E para Easy",
    translate (-100) 0 $ scale 0.3 0.3 $ color yellow $ text "Pressione M para Medium",
    translate (-100) (-50) $ scale 0.3 0.3 $ color red $ text "Pressione H para Hard"
  ]

-- Renderiza o estado do jogo
renderGame :: Game -> Picture
renderGame Menu = desenhaMenu
renderGame (Play snake food _) = pictures $ desenhaCorpo (corpo snake) ++ [desenhaComida food]
  where
    desenhaCorpo = map (\(x, y) -> translate x y $ color green $ rectangleSolid 20 20)
renderGame GameOver = translate (-100) 0 $ scale 0.5 0.5 $ color red $ text "Game Over"

-- Atualiza o estado do jogo
atualizaGame :: Float -> Game -> Game
atualizaGame _ game@(Play snake _ _) 
  | not (state snake) = GameOver
  | otherwise = game
atualizaGame _ game = game

-- Reage à entrada do teclado para mudar a direção da cobra ou iniciar o jogo
handleInput :: Event -> Game -> Game
handleInput (EventKey (Char 'e') _ _ _) Menu = iniciarJogo 1 -- Easy
handleInput (EventKey (Char 'm') _ _ _) Menu = iniciarJogo 3 -- Medium
handleInput (EventKey (Char 'h') _ _ _) Menu = iniciarJogo 5 -- Hard
handleInput (EventKey (SpecialKey KeyUp) _ _ _) (Play snake food over) = Play (snake { snakeDir = UP }) food over
handleInput (EventKey (SpecialKey KeyDown) _ _ _) (Play snake food over) = Play (snake { snakeDir = DOWN }) food over
handleInput (EventKey (SpecialKey KeyLeft) _ _ _) (Play snake food over) = Play (snake { snakeDir = LEFT }) food over
handleInput (EventKey (SpecialKey KeyRight) _ _ _) (Play snake food over) = Play (snake { snakeDir = RIGHT }) food over
handleInput (EventKey (SpecialKey KeyEsc) _ _ _) game = game -- Adiciona uma condição para sair do jogo com ESC
handleInput _ game = game

-- Função para iniciar o jogo com uma determinada velocidade
iniciarJogo :: Float -> Game
iniciarJogo velocidade = Play (Snake [(0,0)] RIGHT velocidade True 0) comida False
  where
    comida = ((0, 0), color red $ circleSolid 10)

main :: IO ()
main = do

  let janela = InWindow "Tabuleiro" (2 * limiteTela, 2 * limiteTela) (500, 500)
  play janela black 10 Menu renderGame handleInput atualizaGame
  putStrLn "Fim do jogo!"  -- Imprime ao sair
  -- Imprime o estado do jogo ao sair (para fins de debug ou análise)
  where
    imprimirEstado game = putStrLn $ "Estado do jogo: " ++ show game
