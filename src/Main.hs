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
data Game = Menu [Picture] StdGen | Play Snake Food [Picture] | GameOver Snake


--Combina todas as posições do jogo para gerar uma lista de pos aleatórias para as frutas
posicaoComida :: [Pos]
posicaoComida = [(x,y) | x <- [(-limiteTela +10), (-limiteTela + 15.0) .. (limiteTela -5)], y <- [(-limiteTela +10), (-limiteTela + 15.0) .. (limiteTela - 5)]]

-- ChatGPT auxiliou para trocar StdGen utilizado anteriormente na função Main para uma geração baseada na posicao da cabeça da cobra
posicaoParaIndice :: Pos -> Int
posicaoParaIndice (x, y) = (mod (floor x + 1000) 1000) * 1000 + (mod (floor y + 1000) 1000)

criaComida :: Pos -> [Picture] -> Food 
criaComida pos frutas = 
   (coord, (resizeImg 0.03 0.03 fruta))
   where 
     coord = posicaoComida !! (posicaoParaIndice pos `mod` length posicaoComida)
     fruta = selecionaAleatorio pos frutas
    
-- Representação gráfica da fruta
desenhaComida :: Food -> Picture
desenhaComida ((x,y), img) =
  translate x y img

-- Funções selecionaAleatorio construída utilizando ChatGPT como ajuda
-- Seleciona fruta aleatoriamente
selecionaAleatorio :: Pos -> [Picture] -> Picture
selecionaAleatorio _ [] = error "Lista de imagens não localizada"
selecionaAleatorio pos frutas = frutas !! (posicaoParaIndice pos `mod` length frutas)
--Ajustatamanho da imagem para ser comportada dentro da tela
resizeImg :: Float -> Float -> Picture -> Picture
resizeImg sx sy img = scale sx sy img 


-- Função que desenha o menu
desenhaMenu :: Picture
desenhaMenu = pictures [
    translate (-300) 100 $ scale 0.5 0.5 $ color white $ text "Escolha a dificuldade:",
    translate (-300) 50 $ scale 0.3 0.3 $ color green $ text "Pressione E para Easy",
    translate (-300) 0 $ scale 0.3 0.3 $ color yellow $ text "Pressione M para Medium",
    translate (-300) (-50) $ scale 0.3 0.3 $ color red $ text "Pressione H para Hard"
  ]

-- Função para desenhar o estado do jogo
renderGame :: Game -> Picture
renderGame (Menu frutas gen) = desenhaMenu
renderGame (GameOver (Snake _ _ _ _ p)) = 
  let gameOverT = "Game Over! Pontuacao: " ++ show p
      textSaida = translate (-350) 0 $ scale 0.4 0.4 $ color red $ text gameOverT
  in textSaida
renderGame (Play (Snake xs _ _ _ _ ) fruta frutas) = 
  pictures [desenhaCobra xs, desenhaComida fruta]

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (Char 'e') _ _ _) (Menu frutas gen) = initGame 4 frutas gen -- Easy
handleEvent (EventKey (Char 'm') _ _ _) (Menu frutas gen) = initGame 7 frutas gen-- Medium
handleEvent (EventKey (Char 'h') _ _ _) (Menu frutas gen) = initGame 12 frutas gen-- Hard
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) (Play snake food frutas) = Play (snake { snakeDir = UP }) food frutas
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) (Play snake food frutas) = Play (snake { snakeDir = DOWN }) food frutas
handleEvent (EventKey (SpecialKey KeyLeft) _ _ _) (Play snake food frutas) = Play (snake { snakeDir = LEFT }) food frutas
handleEvent (EventKey (SpecialKey KeyRight) _ _ _) (Play snake food frutas) = Play (snake { snakeDir = RIGHT }) food frutas
handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) game = game 
handleEvent _ game = game

-- Função de atualização do estado do jogo 
updateGame :: Float -> Game -> Game
updateGame _ (GameOver cobra) = GameOver cobra
updateGame _ (Play (Snake xs dir vel s p) food frutas) = 
  let snakeAtualizada = movimento (Snake xs dir vel s p)
      (snakePosCheck, novaComida) = checaComida snakeAtualizada food frutas
      snakeFinal = checaEx (checaColisao snakePosCheck)
  in if not (s) then GameOver snakeFinal else Play snakeFinal novaComida frutas
updateGame _ game = game


--Função para iniciar o jogo. Usamos o ChatGPT para entender como integrar o Menu com a chamada da função selecionaAleatorio.
initGame :: Float -> [Picture] -> StdGen -> Game
initGame vel frutas g= Play (Snake [(0,0)] UP vel True 0) fruta frutas
  where
  fruta = criaComida coord frutas
  (xGen, yGen) = split g
  convCoord x y = (x * 2 * limiteTela - limiteTela , y * 2 * limiteTela - limiteTela)
  coord = convCoord (head(randoms xGen)) (head (randoms yGen))

-- Checar se a cobra apenas tem a cabeça (evitar erro por causa do init)
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

-- Funcao de movimentacao da cobra
movimento :: Snake -> Snake
movimento (Snake ((x,y):xs) UP vel s p) = Snake ((x, y + 1 * vel):safeInit xs) UP vel s p
movimento (Snake ((x,y):xs) DOWN vel s p) = Snake ((x, y - 1 * vel):safeInit xs) DOWN vel s p
movimento (Snake ((x,y):xs) LEFT vel s p) = Snake ((x - 1 * vel, y):safeInit xs) LEFT vel s p
movimento (Snake ((x,y):xs) RIGHT vel s p) = Snake ((x + 1 * vel, y):safeInit xs) RIGHT vel s p

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
checaComida :: Snake -> Food -> [Picture] -> (Snake, Food)
checaComida (Snake ((cx,cy):xs) dir vel s p) ((fx,fy), img) frutas =
  if dist <= 15.0 
  then (Snake ((fx, fy) : (cx, cy) : xs) dir vel s (p + 10), criaComida (cx,cy) frutas)
  else ((Snake ((cx,cy):xs) dir vel s p), ((fx,fy), img))
  where
    dist = sqrt ((cx - fx)^2 + (cy - fy)^2)

--Criacao da cobra (conjunto de retangulos)
desenhaSegmentos :: Pos -> Picture
desenhaSegmentos (x,y) = translate x y (color green $ rectangleSolid 15.0 15.0)

desenhaCobra :: [Pos] -> Picture
desenhaCobra cobraCorpo = pictures $ map desenhaSegmentos cobraCorpo

main :: IO ()
main = do
  
  
  -- Carregar imagens de frutas (utilizado vídeo disponível em:https://www.youtube.com/watch?v=jtgcJrDQR8U como base para desenvolver a importação das imagens e toda parte de posicionamento das frutas).
--Para ajuste da função, usei a estrutura dada em aula de Monad ("copiando estrutura do Main")
  let frutasArq = ["_pear.bmp", "_orange.bmp", "_watermelon.bmp", "_apple.bmp"]
  frutas <- mapM loadBMP frutasArq
  gen <-getStdGen

  let inicialGame = Menu frutas gen
  play
    janela
    black
    30
    inicialGame
    renderGame
    handleEvent
    updateGame
  where
    janela = InWindow "Snake Game" (2 * limiteTela, 2 * limiteTela) (50,50)