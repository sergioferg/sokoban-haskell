-- Proyecto 1 - Lenguajes de Programación - 2026-1
-- Carlos Paccagnella, CI. 31752534, C1
-- Sergio Gómez, CI. 30142272, C2

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

moveOffset :: Move -> (Int, Int)
moveOffset U = (-1, 0)
moveOffset D = (1, 0)
moveOffset L = (0, -1)
moveOffset R = (0, 1)

isValidMove :: State -> Move -> Bool
isValidMove ((r1, r2), c, cb) m
    | outOfBounds rf     = False
    | rf == c          = not(outOfBounds rf2 || elem rf2 cb)
    | elem rf cb         = not(rf2 == c || outOfBounds rf2 || elem rf2 cb)
    | otherwise          = True
    where
        (m1, m2) = moveOffset m
        rf = (r1+m1, r2+m2)
        rf2 = (r1+(m1*2), r2+(m2*2))

applyMove :: State -> Move -> State
applyMove ((r1, r2), (c1, c2), cb) m
    | rf == c =  (rf, rf2, cb) 
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
        -- bfs :: Cola -> Visitados -> Resultado
        bfs :: [[State]] -> [State] -> (Int, [State])
        bfs [] _ = (0, [])
        bfs (caminoActual : resto) visitados 
            | metaAlcanzada estadoActual = (length caminoActual - 1, reverse caminoActual) 
            | otherwise = bfs nuevoResto nuevoVisitados
            where
                -- Sacamos el estado actual (el primero de la lista)
                estadoActual = head caminoActual 
                
                -- Calculamos los movimientos
                movimientos = [U, D, L, R] 
                movValidos = filter (isValidMove estadoActual) movimientos -- a donde puedo moverme
                estadosResultantes = map (applyMove estadoActual) movValidos  
                estadosNuevos = filter (`notElem` visitados) estadosResultantes 
                
                -- Creamos los caminos nuevos pegando el estado nuevo al camino actual
                nuevosCaminos = map (: caminoActual) estadosNuevos 
                
                -- Actualizamos la cola y los visitados
                nuevoResto = resto ++ nuevosCaminos 
                nuevoVisitados = visitados ++ estadosNuevos


    
