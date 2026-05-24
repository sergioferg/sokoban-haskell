-- Proyecto 1 - Lenguajes de Programación - 2026-1

type Coord = (Int, Int)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord])

eq :: Coord -> Coord -> Bool
eq c1 c2 = c1 == c2

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

moveOffset :: Move -> (Int, Int)
moveOffset U = (-1, 0)
moveOffset D = (1, 0)
moveOffset L = (0, -1)
moveOffset R = (0, 1)

isValidMove :: State -> Move -> Bool
isValidMove ((r1, r2), (c1, c2), cb) m
    | outOfBounds rf     = False
    | rf `eq` c          = not(outOfBounds rf2 || elem rf2 cb)
    | elem rf cb         = not(rf2 `eq` c || outOfBounds rf2 || elem rf2 cb)
    | otherwise          = True
    where
        (m1, m2) = moveOffset m
        rf = (r1+m1, r2+m2)
        c = (c1, c2)
        rf2 = (r1+(m1*2), r2+(m2*2))

applyMove :: State -> Move -> State
applyMove ((r1, r2), (c1, c2), cb) m
    | rf `eq` c =  (rf, rf2, cb)
    | elem rf cb =  (rf, c, rf2 : filter (/= rf) cb) --elementos distintos a rf se quedan, el igual se reemplaza por rf2--
    | otherwise = (rf, c, cb)
    where
        (m1, m2) = moveOffset m
        rf = (r1+m1, r2+m2)
        c = (c1, c2)
        rf2 = (r1+(m1*2), r2+(m2*2))


metaAlcanzada :: State -> Bool
metaAlcanzada (_, coord, _) = coord == (5, 5)


solveWarehouse :: State -> (Int, [State])
solveWarehouse inicial = bfs [[inicial]] [inicial]
    where 
        bfs :: [[State]] -> [State] -> (Int, [State])
        bfs [] _ = (0, [])
        bfs (estadoActual : resto) camino -- la defino así sin razón real por ahora
        | bfs metaAlcanzada actual = (length caminoActual - 1, reverse caminoActual) --segun gemini meter al frente los elementos es mas rapido, por eso uso reverse para imprimir bien

        where
            movimientos = [U, D, L, R] 
            movValidos = filter (isValidMove estadoActual) movimientos -- movimientos validos de los 4 posibles
            estadosResultantes = map (applyMove estadoActual) movValidos --estados nuevos tras hacer las 4 direcciones
            estadosNuevos = filter (`notElem` visitados) estadosResultantes --movimientos para no caer en bucle
            nuevosCaminos = map (: caminoActual) estadosNuevos --unimos el camino con las distintas posibilidades
        


    
