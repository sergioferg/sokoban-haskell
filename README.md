# Proyecto #1 - Haskell

## Almacén Robótico

Para la validación de movimientos y de estado inicial, se creó una funcion auxiliar outOfBounds, que dada una coordenada, retorna True si esta se encuentra fuera del tablero o false en caso contrario.

```
outOfBounds :: Coord -> Bool
outOfBounds (x, y)
    | x < 0 || x > 5 = True
    | y < 0 || y > 5 = True
    | otherwise = False
```

### Parte 1 – Inicialización del Estado

El estado del juego almacena la posición del Robot, la posición de la Caja Objetivo y una lista con las posiciones
de las Cajas de Bloqueo.

```
type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)
```

#### - Uso de `deriving (Show, Eq)`

Cuando se crea un nuevo tipo de dato personalizado usando data, haskell no sabe como
imprimirlo ni como comparar para saber si 2 valores de ese tipo son iguales.
Para evitar tener que programar esas reglas a mano, se utiliza la cláusula `deriving (Show, Eq)` al final de la declaración. 
Esto le dice al compilador de Haskell que genere ese código automáticamente por ti.

Eq: Le enseña a Haskell cómo comparar los elementos del tipo de dato.
Al derivar Eq, se pueden usar los operadores de igualdad (==) y desigualdad (/=).
Haskell simplemente comparará si los componentes internos de ambos datos son idénticos.

Show: Le enseña a Haskell cómo convertir el tipo de dato a texto (String).
Esto es obligatorio si se quiere que el valor se pueda imprimir en la consola (GHCi)
o usar la función show

#### - Verificación de estado inicial

Para la verificación el estado inicial, se creó la funcion `initialState`, que recibe la coordenada inicial del Robot, la de la Caja Objetivo, y la lista de
Cajas de Bloqueo, devolviendo el estado State.

```
initialState :: Coord -> Coord -> [Coord] -> State
```

Utilizamos guardas para verificar que el estado inicial cumpla todas las condiciones propuestas. Definimos dentro de la función el estado invalido que será retornado
en caso de que no se cumpla cualquiera de las condiciones, `invalidState = ((-1, -1), (-1, -1), [])`.

En esta utilizamos la funcion auxiliar `outOfBounds` para verificar que ni el robot ni la caja objetivo, ni ninguna de las
Cajas de Bloqueo se encuentren fuera del tablero.

```
outOfBounds r           = invalidState
outOfBounds c           = invalidState
or (map outOfBounds cb) = invalidState
```

También se chequeó que ninguna de las cajas se solapen entre si.

```
r == c                  = invalidState
elem r cb               = invalidState
elem c cb               = invalidState
```

### Parte 2 - Validación de Movimientos

Antes de ejecutar un movimiento, se verifica si es posible realizarlo siguiendo las reglas del entorno
(límites del tablero, empuje de cajas y colisiones).

Se implementó la función isValidMove, que recibe el estado actual y el movimiento a realizar, devolviendo True si
es válido o False en caso contrario.

```
isValidMove :: State -> Move -> Bool
isValidMove ((r1, r2), c, cb) m
    | outOfBounds rf     = False
    | rf == c            = not(outOfBounds rf2 || elem rf2 cb)
    | elem rf cb         = not(rf2 == c || outOfBounds rf2 || elem rf2 cb)
    | otherwise          = True
    where
        (m1, m2) = moveOffset m
        rf = (r1+m1, r2+m2)
        rf2 = (r1+(m1*2), r2+(m2*2))
```

Para esta validacion de movimientos se utilizó una funcion auxiliar `moveOffset` que dado un movimiento, retorna valores segun el movimiento a realizar
(por ejemplo, en caso de querer moverse hacia arriba 'U', esta retorna -1 en la primera coordenada y 0 en la segunda ya que esto representa el movimiento a realizar).

```
moveOffset :: Move -> (Int, Int)
moveOffset U = (-1, 0)
moveOffset D = (1, 0)
moveOffset L = (0, -1)
moveOffset R = (0, 1)
```

Esta se utiliza para calcular en donde se encontraria el robot en caso de moverse `(m1, m2) = moveOffset m / rf = (r1+m1, r2+m2)` y poder hacer chequeos, en caso de que rf se encuentre fuera
del tablero se retorna false, en caso de que esta coincida con la caja objetivo, se chequea si esta puede ser movida (que la caja no sea empujada fuera del tablero, o que
exista otra caja detras), esta misma logica se aplica a las cajas de bloqueo.

