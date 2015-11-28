module Board where
import Data.Map (Map)
import qualified Data.Map as Map
import Tile

-----------------------------------
data Pos = Pos Int Int deriving (Show)

instance Eq Pos where
    (Pos x1 y1) == (Pos x2 y2) = x1 == x2 && y1 == y2

instance Ord Pos where
  (Pos x1 y1) `compare` (Pos x2 y2) = case x1 `compare` x2 of
                                    EQ -> y1 `compare` y2
                                    _ -> x1 `compare` x2

-----------------------------------
type Board = Map Pos Tile

getTile :: Board -> Pos -> Tile
getTile board pos = case Map.lookup pos board of
                      Just t -> t
                      Nothing -> Tile Grass

type StringBoard = [String]

stringRowToBoard :: String -> Pos -> Board
stringRowToBoard stringRow rowStart =
    let fromRow (c:cs) (Pos x y) = [(Pos x y, fromChar c)] ++ fromRow cs (Pos (x + 1) y)
        fromRow [] (Pos x y) = []
    in Map.fromList (fromRow stringRow rowStart)

stringBoardToBoard :: StringBoard -> Pos -> Board
stringBoardToBoard (row:xr) (Pos x y) = Map.union (stringRowToBoard row (Pos x y)) (stringBoardToBoard xr (Pos x (y + 1)))
stringBoardToBoard [] _ = Map.empty
