module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Direções: Movimentação da cobra
data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

-- Posicao: Utilizado para gerar a posição dos segmentos do corpo da cobra e comida (baseado em https://haskell.pesquisa.ufabc.edu.br/haskell/07.adt/ 2.1 - Tipo Soma)
type Pos = (Float, Float)

--Comida: Posicao em que está representacao gráfica da comida
data Food = Food Pos Picture

--Snake: Representação da cobra no jogo
data Snake = Snake{
  corpo :: [Pos],  -- Armazena todos os "segmentos" que compoem o corpo da cobra
  snakeDir :: Dir, -- Armazena direcao atual da cobra
  speed :: Float, -- Velocidade de movimento
  state :: Bool, -- Armazena o estado da cobra ("viva" ou "morta")
  pontuacao :: Int --Armazena pontuacao
}
 
--Game: Estados do jogo
data Game = Menu { timer :: Float, comida :: Maybe Food } | Play Snake Food | GameOver Snake

-- Type class para objetos que podem ser desenhados
class Drawable a where
    draw :: a -> Picture

-- Implementação de Drawable para Food
instance Drawable Food where
    draw (Food (x, y) img) = translate x y img

-- Implementação de Drawable para Snake
instance Drawable Snake where
    draw (Snake xs _ _ _ _) = desenhaCobra xs

-- Implementação de Drawable para Game
instance Drawable Game where
    draw = renderGame

-- Cria uma comida com posição aleatória
criaComida :: StdGen -> Picture -> Food
criaComida g img =
  Food coord (resizeImg 0.03 0.03 img)
  where 
    (xGen, yGen) = split g
    convCoord x y = (x * 2 * limiteTela - limiteTela , y * 2 * limiteTela - limiteTela)
    coord = convCoord (head(randoms xGen)) (head (randoms yGen))

-- Seleciona fruta aleatoriamente
selecionaAleatorio :: [a] -> IO a
selecionaAleatorio [] = error "Lista de imagens não localizada"
selecionaAleatorio ls = do
  let tamanho = length ls
  indice <- randomRIO (0, tamanho - 1)
  return (ls !! indice)

--Ajustatamanho da imagem para ser comportada dentro da tela
resizeImg :: Float -> Float -> Picture -> Picture
resizeImg sx sy img = scale sx sy img 

-- Função para desenhar o estado do jogo
renderGame :: Game -> Picture
renderGame (Menu _ _) = pictures [
    translate (-350) 180 $ scale 0.5 0.5 $ color white $ text "Bem vindo ao jogo",
    translate (-350) 100 $ scale 0.5 0.5 $ color white $ text "da cobrinha frutifera!",
    translate (-350) 50 $ scale 0.2 0.2 $ color green $ text "Aperte a tecla 'P' para dar play no jogo.",
    translate (-350) (-50) $ scale 0.2 0.2 $ color yellow $ text "Como jogar: use as teclas cima, baixo,",
    translate (-350) (-100) $ scale 0.2 0.2 $ color yellow $ text "esquerda e direita para se movimentar.",
    translate (-350) (-150) $ scale 0.2 0.2 $ color red $ text "Objetivo: comer a maior quantidade de frutas."
  ]
renderGame (GameOver (Snake _ _ _ _ p)) = 
  let gameOverT = "Game Over! Pontuacao: " ++ show p
      textSaida = translate (-400) 0 $ scale 0.4 0.4 $ color red $ text gameOverT
  in textSaida
renderGame (Play snake food) = 
  pictures [draw snake, draw food]

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (Char 'p') Down _ _) (Menu _ (Just comida)) = 
  Play (Snake [(0, 0)] UP 5 True 0) comida
handleEvent _ (GameOver cobra) = GameOver cobra
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) (Play snake food) = Play (snake { snakeDir = UP }) food
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) (Play snake food) = Play (snake { snakeDir = DOWN }) food
handleEvent (EventKey (SpecialKey KeyLeft) _ _ _) (Play snake food) = Play (snake { snakeDir = LEFT }) food
handleEvent (EventKey (SpecialKey KeyRight) _ _ _) (Play snake food) = Play (snake { snakeDir = RIGHT }) food
handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) game = game 
handleEvent _ game = game

-- Função de atualização do estado do jogo 
updateGame :: Float -> Game -> Game
updateGame dt (Menu t comida) = Menu t comida
updateGame _ (GameOver cobra) = GameOver cobra
updateGame _ (Play snake food) = 
  let snakeAtualizada = movimento snake
      snakePosCheck = checaEx (checaColisao snakeAtualizada)
  in if not (state snakePosCheck) 
     then GameOver snakePosCheck 
     else Play snakePosCheck food
updateGame _ game = game

-- Funcao de movimentacao da cobra
movimento :: Snake -> Snake
movimento (Snake ((x,y):xs) dir vel s p) = 
  let newHead = case dir of
        UP    -> (x, y + vel)
        DOWN  -> (x, y - vel)
        LEFT  -> (x - vel, y)
        RIGHT -> (x + vel, y)
  in Snake (newHead : init ((x,y):xs)) dir vel s p  -- Move a cobra e remove o último segmento

--Extremidades da tela 
limiteTela :: Num a => a
limiteTela = 400

-- Checa se a cobra atingiu a extremidade do Tabuleiro
checaEx :: Snake -> Snake
checaEx cobra@(Snake ((x, y):xs) _ _ _ _) 
  | x >= limiteTela || x <= -limiteTela + 10 = cobra { state = False }
  | y >= limiteTela - 10 || y <= -limiteTela + 10 = cobra { state = False }
  | otherwise = cobra


-- Checa se a cobra colidiu com seu corpo
checaColisao :: Snake -> Snake
checaColisao cobra@(Snake (x:xs) _ _ _ _)
  | x `elem` xs = cobra { state = False }
  | otherwise = cobra

-- Checa se a cobra colidiu com comida
checaComida :: Snake -> Food -> (Snake, Food)
checaComida = undefined

--Criacao da cobra (conjunto de retangulos)
desenhaSegmentos :: Pos -> Picture
desenhaSegmentos (x,y) = translate x y (color green $ rectangleSolid 15.0 15.0)

desenhaCobra :: [Pos] -> Picture
desenhaCobra cobraCorpo = pictures $ map desenhaSegmentos cobraCorpo

main :: IO ()
main = do

  -- Carregar imagens de frutas (utilizado vídeo disponível em:https://www.youtube.com/watch?v=jtgcJrDQR8U como base para desenvolver a importação das imagens e toda parte de posicionamento das frutas)
  let frutasArq = ["_pear.bmp", "_orange.bmp", "_watermelon.bmp", "_apple.bmp"]
  frutas <- mapM loadBMP frutasArq

  --Gerador aleatório
  gen <- getStdGen

  --Gera comida aleatória a partir das imagens importadas
  img <- selecionaAleatorio frutas
  let food = Just (criaComida gen img)
  let inicialGame = Menu 1000 food -- Inicializa o estado com comida
  play
    janela
    black
    30
    inicialGame
    draw  -- Usa a função 'draw' da type class 'Drawable'
    handleEvent
    updateGame
  where
    janela = InWindow "Tabuleiro" (2 * limiteTela, 2 * limiteTela) (50,50)