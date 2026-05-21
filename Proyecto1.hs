-- Proyecto 1 - Lenguajes de Programación - 2026-1

type Coord = (Int, Int)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord])

outOfBounds :: Coord -> Bool
outOfBounds (x, y)
    | x < 0 || x > 5 = True
    | y < 0 || y > 5 = True
    | otherwise = False

initialState :: Coord -> Coord -> [Coord] -> State
initialState r c cb
    | r == c = ((-1, -1), (-1, -1), [])
    | elem r cb = ((-1, -1), (-1, -1), [])
    | elem c cb = ((-1, -1), (-1, -1), [])
    | outOfBounds r = ((-1, -1), (-1, -1), [])
    | outOfBounds c = ((-1, -1), (-1, -1), [])
    | or (map outOfBounds cb) = ((-1, -1), (-1, -1), [])
    | otherwise = (r, c, cb)
