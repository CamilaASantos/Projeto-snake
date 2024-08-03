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
}

--Game: Estados do jogo
data Game = Game{
  gameSnake :: Snake,
  gameFood :: Food,
  gameOver :: Bool,
} deriving (Eq, Show)

-- Funcao de movimentacao da cobra
movimento :: Snake -> Dir -> Snake
movimento  = undefined

--Extremidades da tela 
limiteTela :: Num a -> a
limiteTela = 400

-- Checa se a cobra atingiu a extremidade do Tabuleiro
checaEx :: Snake -> Snake
checaEx cobra@(Snake ((x,y):xs) _ _ estado _ ) =
  |x > limiteTela || x < -limiteTela  = cobra{state = False}
  |y > limiteTela || y < -limiteTela  = cobra{state = False}
  |otherwise = cobra

-- Checa se a cobra  colidiu com seu corpo 
checaColisao :: Snake -> Dir -> Snake
checaColisao cobra@(Snake ((x,y):xs) _ _ estado _ ) d = undefined

-- Representação gráfica da fruta
desenhaComida :: Food -> Picture
desenhaComida ((x,y), img) =
  translate x y img

-- Função construída utilizando ChatGPT como ajuda
selecionaAleatorio :: [a] -> IO a
selecionaAleatorio [] = error "Lista de imagens não localizada"
selecionaAleatorio ls = do
  let tamanho = length ls
  indice <-randomRIO (0, tamanho - 1)
  return (ls !! indice)

criaComida :: StdGen -> Picture -> Food 
criaComida g img =
  (coords, img)
  where 
    (xGen, yGen) = split g
    convCoord x y = (x * 2 * limiteTela - limiteTela , y * 2 * limiteTela - limiteTela)
    coords = zipWith convCoord (randoms xGen) (randoms yGen)
      
main :: IO ()
main = do

  -- Carregar imagens de frutas (utilizado vídeo disponível em:https://www.youtube.com/watch?v=jtgcJrDQR8U como base para desenvolver a importação das imagens e toda parte de posicionamento das frutas)
  let frutasArq = ["_pear.png", "_orange.png", "_watermelon.png", "_apple.png"]
  frutas <- mapM loadPNG frutasArq
  
  gen <- getStdGen

  let selecionaFruta = selecionaAleatorio frutas
  img <- selecionaFruta
  let comida = criaComida gen img
  let cobra = Snake [(0,0)] UP 10 True 0 -- configuracoes iniciais

  simulate
    janela
    black
    60
    (Game (Snake [(0,0)] RIGHT 1 True 0) comida False)
    desenhaComida
    renderGame

  where
    janela = InWindow "Tabuleiro" (2 * limiteTela, 2 * limiteTela) (50,50)
      

