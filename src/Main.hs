module Main where

{-
Projeto entrega 1:

[1] Criar tipos de dados voltados para o Jogo: Camila
    Direcao; OK
    Snake; OK
    Posicao; OK
    Comida; OK
    Game; OK
[2] Criar função de movimentação: Camila
[3] Criar leitura do teclado Fernanda
[4] Criar Menu de seleção para o usuário Fernanda
[5] Saída do estado ("Printar" estado do jogo) Fernanda
-}

-- Direções: Movimentação da cobra
data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord, Show)

-- Posicao: Utilizado para mostrar a posição dos segmentos do corpo da cobra e comida
data Pos = Int Int deriving (Eq, Ord, Show)

--Comida: Posicao em que está 
type Food = Pos

--Snake: Representação da cobra no jogo
data Snake = Snake{
    corpo :: Pos,
    snakeDir :: Dir,
    state :: Int,
    Pontos :: Int
}

--Game: Estados do jogo
data Game = Game{
    gameSnake :: Snake,
    gameFood :: Food,
    gameOver :: Bool,
} deriving (Eq, Show)

main :: IO ()
main = do                  
  