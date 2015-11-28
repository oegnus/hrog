module Tile where

data TileType = MsgBorder | Fog | Player | Grass | Tree | Bridge | Water | Rock | Wall | Road | EmptyTile deriving (Eq, Show)

data Tile = Tile { tileType :: TileType
                 } deriving (Show)

fromChar :: Char -> Tile
fromChar c =
    Tile (case c of
        '-' -> MsgBorder
        '#' -> Fog
        '@' -> Player
        ',' -> Grass
        'A' -> Tree
        '=' -> Bridge
        '~' -> Water
        'o' -> Rock
        'H' -> Wall
        '%' -> Road
        _ -> EmptyTile
    )

toChar :: Tile -> Char
toChar (Tile tileType) =
    case tileType of
         MsgBorder -> '-'
         Fog -> '#'
         Player -> '@'
         Grass -> ','
         Tree -> 'A'
         Bridge -> '='
         Water -> '~'
         Rock -> 'o'
         Wall -> 'H'
         Road -> '%'
