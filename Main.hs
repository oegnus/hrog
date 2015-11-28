module Main where
import System.IO
import Control.Applicative
import qualified Data.Map as Map
import Tile
import Board
import Draw
import MapData

main :: IO ()
main = do
    let board = stringBoardToBoard mapData (Pos 0 0)
    hSetBuffering stdin NoBuffering
    gameLoop board (Pos 1 1) ' '

gameLoop :: Board -> Pos -> Char -> IO()
gameLoop board playerPosition input =
    let destination = move playerPosition input
        newPlayerPosition | isPositionAllowed board destination = destination
                          | otherwise = playerPosition
        viewBounds = playerSorroundings 16 8 newPlayerPosition
        boardWithFog = drawFog viewBounds newPlayerPosition board
        boardWithFogAndPlayer = putTile (Tile Player) boardWithFog newPlayerPosition
        --boardWithFogPlayerAndMsg = drawMsg (Pos 3 3, Pos 5 5) boardWithFogAndPlayer
    in do
        clearScreen
        putStr $ "\n" ++ printBoard viewBounds boardWithFogAndPlayer
        newInput <- getChar
        gameLoop board newPlayerPosition newInput

canSeeThroughTile (Tile tileType) = elem tileType [Grass, Bridge, Water, Road]

canWalkOnTile (Tile tileType) = elem tileType [Grass, Bridge, Road]

canSeeThroughPosition :: Board -> Pos -> Bool
canSeeThroughPosition board pos = canSeeThroughTile $ getTile board pos

canWalkOnPosition :: Board -> Pos -> Bool
canWalkOnPosition board pos = canWalkOnTile $ getTile board pos

move :: Pos -> Char -> Pos
move (Pos x y) input = case input of
                    'w' -> Pos x (y - 1)
                    'a' -> Pos (x - 1) y
                    's' -> Pos x (y + 1)
                    'd' -> Pos (x + 1) y
                    _ -> Pos x y

playerSorroundings :: Int -> Int -> Pos -> Bounds
playerSorroundings w h (Pos x y) = (Pos (x - w) (y - h), Pos (x + w) (y + h))

isPositionAllowed :: Board -> Pos -> Bool
isPositionAllowed board pos = canWalkOnPosition board pos

intDivide :: Int -> Int -> Int
intDivide a b = round(fromIntegral a / fromIntegral b)

makeLine :: Pos -> Pos -> [Pos]
makeLine (Pos x1 y1) (Pos x2 y2) =
   let dx = x2 - x1
       dy = y2 - y1
   in case abs dx > abs dy of
      True -> map (\x -> Pos x (y1 + (intDivide (dy * (x - x1)) dx))) [(min x1 x2)..(max x1 x2)]
      False -> map (\y -> Pos (x1 + (intDivide (dx * (y - y1)) dy)) y) [(min y1 y2)..(max y1 y2)]

isTileVisible :: Board -> Pos -> Pos -> Bool
isTileVisible board playerPos tilePos =
    let pointsBetween = init $ tail (makeLine playerPos tilePos)
        isNotTransparent pos = not $ canSeeThroughPosition board pos
    in length (filter isNotTransparent pointsBetween) == 0

drawFog :: Bounds -> Pos -> Board -> Board
drawFog (Pos x1 y1, Pos x2 y2) playerPos board =
    let allPositions = map (\(x, y) -> Pos x y) $ (,) <$> [x1..x2] <*> [y1..y2]
        tile tilePos | isTileVisible board playerPos tilePos = getTile board tilePos
                     | otherwise = Tile Fog
    in Map.fromList $ map (\pos -> (pos, tile pos)) allPositions

drawMsg :: Bounds -> Board -> Board
drawMsg (Pos x1 y1, Pos x2 y2) board =
    let allPositions = map (\(x, y) -> Pos x y) $ (,) <$> [x1..x2] <*> [y1..y2]
    in foldl
        (putTile (Tile MsgBorder))
        board
        allPositions

putTile :: Tile -> Board -> Pos -> Board
putTile tile board pos = Map.insert pos tile board
