module Draw where
import Tile
import Board
import qualified Data.Map as Map

type Bounds = (Pos, Pos)

colors = Map.fromList [ ("darkGray", "30")
                      , ("darkGrayBg", "40")
                      , ("red", "31")
                      , ("redBg", "41")
                      , ("green", "32")
                      , ("greenBg", "42")
                      , ("yellow", "33")
                      , ("yellowBg", "43")
                      , ("blue", "34")
                      , ("blueBg", "44")
                      , ("purple", "35")
                      , ("purpleBg", "45")
                      , ("teal", "36")
                      , ("tealBg", "46")
                      , ("gray", "37")
                      , ("grayBg", "47")
                      ]

color name str = case Map.lookup name colors of
    Just colorCode -> "\x1b[" ++ colorCode ++ "m" ++ str ++ "\x1b[0m"
    Nothing -> str

printTile (Tile t) = case t of
    Player -> color "redBg" "I"
    Grass -> color "green" $ color "darkGrayBg" ","
    Tree -> color "green" $ color "darkGrayBg" "A"
    Rock -> color "yellow" $ color "darkGrayBg" "o"
    Water -> color "blueBg" "~"
    Bridge -> color "darkGray" $ color "yellowBg" "="
    Fog -> color "darkGray" "."
    Wall -> color "teal" $ color "grayBg" "H"
    Road -> color "darkGray" $ color "darkGrayBg" "%"
    MsgBorder -> color "blue" $ color "grayBg" "-"
    otherwise -> "%"

printBoard :: Bounds -> Board -> String
printBoard (Pos x1 y1, Pos x2 y2) board =
    let tile x y = printTile $ getTile board (Pos x y)
    in [y1..y2] >>= \y ->
       ([x1..x2] >>= \x ->
       tile x y) ++ "\n"

clearScreen = putStr "\ESC[2J"

printColor num = "\x1b[" ++ (show num) ++ "m" ++ "x" ++ "\x1b[0m"
printAllColors = concat $ map printColor [30..37]
printAllBgColors = concat $ map printColor [40..47]
