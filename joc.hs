import System.Random
import Data.Vector as Vec

-- DECLARACIONES DE TIPOS Y DATA -----------------------------------------------------
-- Types
type Posicion = Maybe Ficha
type Columna = Vec.Vector Posicion
type Tablero = Vec.Vector Columna
type Jugador = Ficha

-- Data
data Ficha = X | O
data PartidaTerminada = NoTerminada | Ganador Jugador | Empate

-- FUNCIONES AUXILIARES --------------------------------------------------------------
-- Selección de la estategia de la IA
chooseIA :: Maybe IO Int 
chooseIA = do
    putStrLn "Choose the difficultity:"
    putStrLn "1. Random"
    putStrLn "2. Greedy"
    putStrLn "3. Smart"
    x <- getLine
    return (read x)

-- Entrada de la jugada
leerJugada :: Bool -> IO Int
leerJugada Nothing = do         -- Usuario
    putStr "Enter your move: "
    x <- getLine
    return (read x)

leerJugada Just ia = do         -- IA    
    -- Llamada a la IA
    -- De momento los haremos como 2 usuarios
    putStr "Enter your move: "
    x <- getLine
    return (read x)

printWinner :: Jugador -> IO ()
printWinner jugador = do
    putStrLn "HA GANADO EL JUGADOR"

printDraw :: IO ()
printDraw = do
    putStrLn "EMPATE"


-- MODULOS ---------------------------------------------------------------------------
-- Implementacion del tablero de juego
module Tablero(
    --Atributos
    Tablero,
    Ficha(..),
    Jugador,
    numCols, numFilas, 
    --METODOS
    --Creacion de un tablero vacio
    tableroVacio :: Tablero
    
    --Transformacion de posiciones de vectores a lista
    columnaLista :: Tablero -> Int -> [Posicion]
    filaLista :: Tablero -> Int -> [Posicion]
    diagUpLista ::  Tablero -> Int -> Int -> [Posicion]
    diagDownLista :: Tablero -> Int -> Int -> [Posicion]

    getPosFromVec :: Vec.Vector Posiciones -> Int -> Posicion

    --Obtencion de la posicion dadas las coordenadas
    getPosicion :: Tablero -> Int -> Int -> Posicion

    --Poner ficha de un jugador en una columna
    ponerFicha :: Tablero -> Jugador -> Int -> Maybe Tablero

    movimientoValido :: Board -> Int -> Bool

    --Dice si existen 4 piezas del mismo jugador seguidas
    cuatroEnRaya :: [Posicion] -> Maybe Jugador

    --PRINTS
    printFila :: Vec.Vector -> IO ()
    printTablero :: Tablero -> IO ()




) where

    tableroVacio = Vec.replicate 7 $ Vec.replicate 6 Nothing

    getPosFromVec columna fila = do
        return (columna !! fila)

    columnaLista tablero col = do
        return (V.toList . tablero !! col)

    filaLista tablero fila = do
        auxfila <- Vec.fromList [0..5]
        return (Vec.toList ((Vec.map (!auxFila) tablero) !! fila))
                

    diagDownLista tablero fila col = do
        return ListaDiag
        where
            ListaDiag = do 

    diagUpLista tablero fila columna = do
        return ListaDiag
        where
            ListaDiag = do

    getPosicion tablero fila columuna = do
        

    ponerFicha tablero jugador columna = do
        -- Mirar si columna es válida

        -- 

    movimientoValido tablero columna = do


    cuatroEnRaya [] = Nothing
    cuatroEnRaya (v:w:x:y:z)
        | v == w && w == x && x == y = Just x
        | otherwise = cuatroEnRaya (w:x:y:z)

    printFila fila = do
        V.mapM_ (putChar . maybe '.' (head . show)) fila >> putStrLn ""
        
    printTablero tablero = do
        mapM_ (putStr . show) [0..6]
        putStrLn ""
        V.mapM_ printFila transTablero tablero
            -- Haremos la transpuesta del tablero para que sea más facil de imprimir
            where transTablero tablero1 = do
                fila <- V.fromList [0..6]
                return (V.map (!fila) tablero1)


-- Implementacion del estado de la partida
module Estado(
    --Atributos
    _tablero :: Tablero
    _turno :: Jugador
    _terminado :: PartidaTerminada

    --Metodos
    getTablero :: Estado -> Tablero
    getTurno :: Estado -> Jugador
    getTerminado :: Estado -> PartidaTerminada

    switchTurno :: Estado -> 
    creaEstado :: Tablero -> Jugador -> PartidaTerminada -> Estado

    sigJugador :: Jugador -> Jugador

) where
    getTablero estado = estado._tablero
    getTurno estado = estado._turno
    getTerminado estado = estado._terminado
    getUltMov estado = estado._ultimoMovimiento

    sigJugador X = O
    sigJugador O = X


-- ESTRUCTURA PRINCIPAL DEL JUEGO ----------------------------------------------------
-- Llamada inicial
main :: IO ()
main = do
    main = do
    diff <- chooseIA         --Seleccion de la IA
    let t = rand 0 1        --Generación del turno inicial
        if(t == 0) then turn = false
        else turn = true
    --Crear tablero vacio
    board <- tableroVacio
    --Llamar bucle de juego
    estado <- creaEstado board t NoTerminada 
    play estado

-- Bucle principal del juego
play :: Estado -> Estado 
play estado = do
    haTerminado <- getTerminado estado
    
    --Partida empatada
    if (haTerminado == Empate) then do
        printDraw
    
    if (haTerminado == NoTerminada) then do
        --Leer entrada de datos del jugador (usuario o IA)
        jugada <- leerJugada
        --Ejecutar movimiento
        jugador <- getTurno estado
        tablero <- getTablero estado
        newEstado <- ponerFicha tablero jugador jugada
        setTurno estado 
        --Comprobar si el movimiento es ganador
        do cuatroEnRaya . columnaLista (getTablero estado)
        do cuatroEnRaya . filaLista 
        do cuatroEnRaya . diagUpLista
        do cuatroEnRaya . diagDownLista
        si alguna es valida -> PartidaTerminada = Ganador jugador
        si ninguna es valida -> PartidaTerminada = NoTerminada
        --Recursividad
        play newEstado

    else do
        -- Tenemos ganador
        ganador <- snd . haTerminado
        printWinner ganador

        