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
data Game = Menu | Play Snake Food | GameOver 
-- Funcao de movimentacao da cobra
movimento :: Snake -> Dir -> Snake
movimento cobra UP = cobra{snakeDir = UP}
movimento cobra DOWN = cobra{snakeDir = DOWN}
movimento cobra LEFT = cobra{snakeDir = LEFT}
movimento cobra RIGHT = cobra{snakeDir = RIGHT}

--Extremidades da tela 
limiteTela :: Num a => a
limiteTela = 400

-- Checa se a cobra atingiu a extremidade do Tabuleiro
checaEx :: Snake -> Snake
checaEx cobra@(Snake ((x, y):xs) _ _ _ _) 
  | x >= limiteTela || x <= -limiteTela = cobra { state = False }
  | y >= limiteTela || y <= -limiteTela = cobra { state = False }
  | otherwise = cobra


-- Checa se a cobra colidiu com seu corpo
checaColisao :: Snake -> Snake
checaColisao cobra@(Snake (x:xs) _ _ _ _)
  | x `elem` xs = cobra { state = False }
  | otherwise = cobra

-- Checa se a cobra colidiu com comida
--checaComida :: Snake -> Food -> Snake
--checaComida =undefined

-- Representação gráfica da fruta
desenhaComida :: Food -> Picture
desenhaComida ((x,y), img) =
  translate x y img

-- Funções selecionaAleatorio e resizeImg construídas utilizando ChatGPT como ajuda (manipulação da biblioteca Gloss)
-- Seleciona fruta aleatoriamente
selecionaAleatorio :: [a] -> IO a
selecionaAleatorio [] = error "Lista de imagens não localizada"
selecionaAleatorio ls = do
  let tamanho = length ls
  indice <-randomRIO (0, tamanho - 1)
  return (ls !! indice)

--Ajustatamanho da imagem para ser comportada dentro da tela
resizeImg :: Float -> Float -> Picture -> Picture
resizeImg sx sy img = scale sx sy img 

criaComida :: StdGen -> Picture -> Food 
criaComida g img =
  (coord, (resizeImg 0.04 0.04 img))
  where 
    (xGen, yGen) = split g
    convCoord x y = (x * 2 * limiteTela - limiteTela , y * 2 * limiteTela - limiteTela)
    coord = convCoord (head(randoms xGen)) (head (randoms yGen))

-- Função para desenhar o estado do jogo
renderGame :: Game -> Picture
renderGame (Play _ fruta) = 
  pictures [ desenhaComida (fruta) ]

handleEvent :: Event -> Game -> Game
handleEvent _ game = game

-- Função de atualização do estado do jogo
updateGame :: Float -> Game -> Game
updateGame _ game = game

main :: IO ()
main = do

  -- Carregar imagens de frutas (utilizado vídeo disponível em:https://www.youtube.com/watch?v=jtgcJrDQR8U como base para desenvolver a importação das imagens e toda parte de posicionamento das frutas)
  let frutasArq = ["_pear.bmp", "_orange.bmp", "_watermelon.bmp", "_apple.bmp"]
  frutas <- mapM loadBMP frutasArq
  
  --Gerador aleatório
  gen <- getStdGen

  --Gera comida aleatória a partir das imagens importadas
  let selecionaFruta = selecionaAleatorio frutas
  img <- selecionaFruta
  let comida = criaComida gen img
  let cobra = Snake [(0,0)] UP 10 True 0-- configuracoes iniciais
  let inicialGame = Play cobra comida
  play
    janela
    black
    30
    inicialGame
    renderGame
    handleEvent
    updateGame
  where
    janela = InWindow "Tabuleiro" (2 * limiteTela, 2 * limiteTela) (50,50)
    

