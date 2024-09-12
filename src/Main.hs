module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Control.Monad.State
import Debug.Trace (trace)

--Game: Estados do jogo
data Game = Menu | Play | GameOver deriving (Eq)

--Construção de todos os elementos do jogo - Refatoração usando Monada de estado
data GameState = GameState {
  snake :: Snake, -- Armazena cobra 
  foods :: [Picture], -- Lista de imagens para frutas
  food :: Food, -- Armazena fruta 
  pontuacao :: Int, --Armazena pontuacao
  estadoGame :: Game, -- Armazena o estado do jogo
  randomGen :: StdGen  -- Gerador de números aleatórios
} 

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
  speed :: Float -- Velocidade de movimento
} deriving (Show)
 
 --Combina todas as posições do jogo para gerar uma lista de pos aleatórias para as frutas
posicaoComida :: [Pos]
posicaoComida = [(x,y) | x <- [(-limiteTela +10.0), (-limiteTela + 20.0) .. (limiteTela -10.0)], y <- [(-limiteTela +10.0), (-limiteTela + 20.0) .. (limiteTela - 10.0)]]

-- ChatGPT auxiliou para trocar StdGen utilizado anteriormente na função Main para uma geração baseada na posicao da cabeça da cobra
posicaoParaIndice :: Pos -> Int
posicaoParaIndice (x, y) = (mod (floor x + 1000) 1000) * 1000 + (mod (floor y + 1000) 1000)

-- Funções selecionaAleatorio construída utilizando ChatGPT como ajuda
-- Seleciona fruta aleatoriamente
selecionaAleatorio :: Pos -> [Picture] -> Picture
selecionaAleatorio _ [] = error "Lista de imagens não localizada"
selecionaAleatorio pos frutas = frutas !! (posicaoParaIndice pos `mod` length frutas)

criaComida :: State GameState Food 
criaComida  = do
  gState <- get
  let coord = posicaoComida !! (posicaoParaIndice pos `mod` length posicaoComida)
      frutas = foods gState
      pos = head $ corpo (snake gState)
      fruta = selecionaAleatorio pos frutas
  return (coord, (resizeImg 0.02 0.02 fruta))

--Ajusta tamanho da imagem para ser comportada dentro da tela
resizeImg :: Float -> Float -> Picture -> Picture
resizeImg sx sy img = scale sx sy img 

-- Representação gráfica da fruta
desenhaComida :: Food -> Picture
desenhaComida ((x,y), img) =
  translate x y img

--Criacao da cobra (conjunto de retangulos)
desenhaSegmentos :: Pos -> Picture
desenhaSegmentos (x,y) = translate x y (color green $ rectangleSolid 10.0 10.0)

desenhaCobra :: [Pos] -> Picture
desenhaCobra cobraCorpo = pictures $ map desenhaSegmentos cobraCorpo

-- Função que desenha o menu
desenhaMenu :: Picture
desenhaMenu = pictures [
    translate (-300) 100 $ scale 0.5 0.5 $ color white $ text "Escolha a dificuldade:",
    translate (-300) 50 $ scale 0.3 0.3 $ color green $ text "Pressione E para Easy",
    translate (-300) 0 $ scale 0.3 0.3 $ color yellow $ text "Pressione M para Medium",
    translate (-300) (-50) $ scale 0.3 0.3 $ color red $ text "Pressione H para Hard"
  ]

renderGame :: GameState -> Picture
renderGame gState = evalState renderGame' gState

-- Função para desenhar o estado do jogo
renderGame' :: State GameState Picture
renderGame' = do
  gState <- get
  case estadoGame gState of
    Menu -> return desenhaMenu
    GameOver -> 
      let gameOverT = "Game Over! Pontuacao: " ++ show (pontuacao gState)
          textSaida = translate (-350) 0 $ scale 0.4 0.4 $ color red $ text gameOverT
      in return textSaida
    Play -> do
      let (Snake xs _ _) = snake gState
          fruta = food gState
      return $ pictures [desenhaCobra xs, desenhaComida fruta]

handleEvent :: Event -> GameState -> GameState
handleEvent event gState = execState (handleEvent' event) gState

handleEvent' :: Event -> State GameState ()
handleEvent' (EventKey (Char 'e') _ _ _) = do
  frutas <- gets foods
  estadoJogo <-gets estadoGame
  if estadoJogo == Menu || estadoJogo == GameOver
     then do
       initGame 0.5 frutas-- Easy
     else 
       return ()
handleEvent' (EventKey (Char 'm') _ _ _) = do
  frutas <- gets foods
  estadoJogo <-gets estadoGame
  if estadoJogo == Menu || estadoJogo == GameOver
     then do
       initGame 0.8 frutas -- Medium
     else 
       return ()
handleEvent' (EventKey (Char 'h') _ _ _) = do
  frutas <- gets foods
  estadoJogo <-gets estadoGame
  if estadoJogo == Menu || estadoJogo == GameOver
     then do
       initGame 1.0 frutas -- Hard
     else 
       return ()
handleEvent' (EventKey (SpecialKey KeyUp) _ _ _)   = moveDirecao UP
handleEvent' (EventKey (SpecialKey KeyDown) _ _ _) = moveDirecao DOWN
handleEvent' (EventKey (SpecialKey KeyLeft) _ _ _) = moveDirecao LEFT
handleEvent' (EventKey (SpecialKey KeyRight) _ _ _) = moveDirecao RIGHT
handleEvent' (EventKey (SpecialKey KeyEsc) _ _ _) = return () 
handleEvent' _ =  return ()

-- ChatGPT auxiliou com os erros recebidos da função play do gloss para atuar com as monadas, sendo necessário este ajuste nas funções principais
updateGameState :: Float -> GameState -> GameState
updateGameState deltaTime gState = execState (updateGameState' deltaTime) gState

-- Função de atualização do estado do jogo 
updateGameState' :: Float -> State GameState ()
updateGameState' _ = do
  moveSnake
  checaEx
  checaColisao
  checaComida

initGame :: Float -> [Picture] -> State GameState ()
initGame vel frutas = do
  gState <- get
  let snakeInicial = Snake [((20.0), (10.0)), ((10.0), (10.0)),((0.0), (10.0))] UP vel
      gen = randomGen gState
      (xGen, yGen) = split gen
      convCoord x y = (x * 2 * limiteTela - limiteTela , y * 2 * limiteTela - limiteTela)
      coord = convCoord (head(randoms xGen::[Float])) (head (randoms yGen::[Float]))
      fruta = (coord, (resizeImg 0.02 0.02 (selecionaAleatorio coord frutas)))
  put gState{
    snake = snakeInicial,
    foods = frutas,
    food = fruta,
    pontuacao = 0,
    estadoGame = Play
  }

-- Checar se a cobra apenas tem a cabeça (evitar erro por causa do init)
safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

moveDirecao :: Dir -> State GameState ()
moveDirecao dir = do
  gState <- get  
  let snakeAtual = snake gState
      snakeAtualizada = snakeAtual { snakeDir = dir }
  put gState { snake = snakeAtualizada }

-- Funcao de movimento com Monada
moveSnake :: State GameState ()
moveSnake = do
  gState <- get
  let snakeAtualizada = movimento (snake gState)
  put gState { snake = snakeAtualizada}

-- Funcao de movimentacao da cobra
{-
movimento :: Snake -> Snake
movimento (Snake ((x,y):xs) UP vel) = Snake ((x, y + 1 * vel):safeInit xs) UP vel 
movimento (Snake ((x,y):xs) DOWN vel) = Snake ((x, y - 1 * vel):safeInit xs) DOWN vel
movimento (Snake ((x,y):xs) LEFT vel) = Snake ((x - 1 * vel, y):safeInit xs) LEFT vel
movimento (Snake ((x,y):xs) RIGHT vel) = Snake ((x + 1 * vel, y):safeInit xs) RIGHT vel
-}

movimento :: Snake -> Snake
movimento (Snake ((x,y):xs) dir vel) = 
  let nHead = newHead (x,y) dir vel
      newTailV1 = safeInit ((x,y):xs)
      newTailV2 = safeInit $ (newHead (x,y) dir 1):newTailV1
      newTailV3 = safeInit $ (newHead (x,y) dir 2):newTailV2
      --newTail = if vel == 1.0 then newTailV1 else if vel ==2.0 then newTailV2 else newTailV3
      newBody = nHead:newTailV1
  in (Snake newBody dir vel)

newHead :: Pos -> Dir -> Float -> Pos
newHead (x,y) UP v = (x, y + v * 10)
newHead (x,y) DOWN v = (x, y - v * 10)
newHead (x,y) LEFT v = (x - v * 10, y)
newHead (x,y) RIGHT v = (x + v * 10, y)

--Extremidades da tela 
limiteTela :: Num a => a
limiteTela = 400

-- Checa se a cobra atingiu a extremidade do Tabuleiro
checaEx :: State GameState ()
checaEx = do
  gState <- get
  let (x, y) = head $ corpo (snake gState)
      morreu = x >= limiteTela || x <= -limiteTela + 10 || y >= limiteTela - 10 || y <= -limiteTela + 10
  if morreu
    then do
      put gState {estadoGame = GameOver}
    else return ()

-- Checa se a cobra colidiu com seu corpo
checaColisao :: State GameState ()
checaColisao = do
  gState <- get
  let (x:xs) = corpo (snake gState)
      morreu = x `elem` xs
  if morreu
    then do
      put gState {estadoGame = GameOver}
    else return ()

-- Checa se a cobra colidiu com comida

checaComida :: State GameState ()
checaComida = do
  gState <- get
  let (Snake ((cx, cy):xs) dir vel) = snake gState
      ((fx, fy), _) = food gState
      dist = sqrt ((cx - fx) ^ 2 + (cy - fy) ^ 2)
  if dist <= 15.0
    then do
      novaFruta <- criaComida
      put gState { snake = Snake ((newHead (newHead(cx,cy) dir vel) dir vel) : (cx, cy) : xs) dir vel,food = novaFruta, pontuacao = pontuacao gState + 10 }
  else return ()

main :: IO ()
main = do
-- Carregar imagens de frutas (utilizado vídeo disponível em:https://www.youtube.com/watch?v=jtgcJrDQR8U como base para desenvolver a importação das imagens e toda parte de posicionamento das frutas).
  let frutasArq = ["_pear.bmp", "_orange.bmp", "_watermelon.bmp", "_apple.bmp"]
  frutas <- mapM loadBMP frutasArq
  gen <-getStdGen
  let inicialGame  = GameState {
        snake = Snake [(0,0)] UP 0,  
        foods = frutas,
        food = ((100,100),head (frutas)),
        pontuacao = 0,
        estadoGame = Menu , 
        randomGen = gen
      }
  play
    janela
    black
    30
    inicialGame
    renderGame
    handleEvent
    updateGameState
  where
    janela = InWindow "Snake Game" (2 * limiteTela, 2 * limiteTela) (50,50)