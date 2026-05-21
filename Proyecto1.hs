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
    | r == c                  = invalidState
    | elem r cb               = invalidState
    | elem c cb               = invalidState
    | outOfBounds r           = invalidState
    | outOfBounds c           = invalidState
    | or (map outOfBounds cb) = invalidState
    | otherwise               = (r, c, cb)
        where invalidState = ((-1, -1), (-1, -1), [])
