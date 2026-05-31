-- Proyecto 1 - Lenguajes de Programación - 2026-1
-- Carlos Paccagnella, CI. 31752534, C1
-- Sergio Gómez, CI. , C2

{-
Uso de deriving

Cuando se crea un nuevo tipo de dato personalizado usando data, haskell no sabe como
imprimirlo ni como comparar para saber si 2 valores de ese tipo son iguales
Para evitar tener que programar esas reglas a mano,
 se utiliza la cláusula deriving al final de la declaración. 
 Esto le dice al compilador de Haskell que genere ese código automáticamente por ti.

Eq: Le enseña a Haskell cómo comparar los elementos del tipo de dato.
Al derivar Eq, se pueden usar los operadores de igualdad (==) y desigualdad (/=).
Haskell simplemente comparará si los componentes internos de ambos datos son idénticos.

Show: Le enseña a Haskell cómo convertir el tipo de dato a texto (String).
Esto es obligatorio si se quiere que el valor se pueda imprimir en la consola (GHCi)
o usar la función show
-}

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
        -- bfs :: Cola -> Visitados -> Resultado
        bfs :: [[State]] -> [State] -> (Int, [State])
        bfs [] _ = (0, [])
        bfs (caminoActual : restoCola) visitados 
            | metaAlcanzada estadoActual = (length caminoActual - 1, reverse caminoActual) 
            | otherwise = bfs nuevaCola nuevoVisitados
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
                nuevaCola = restoCola ++ nuevosCaminos 
                nuevoVisitados = visitados ++ estadosNuevos


    
