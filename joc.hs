import System.Random
import Data.Vector as Vec

-- DECLARACIONES DE TIPOS Y DATA -----------------------------------------------------
-- Types
type Posicion = Maybe Ficha
type Fila = Vec.Vector Posicion
type Tablero = Vec.Vector Fila
type Jugador = Ficha

-- Data
data Ficha = X | O
data PartidaTerminada = NoTerminada | Ganador Jugador | Empate

-- FUNCIONES AUXILIARES --------------------------------------------------------------
-- A que jugador le toca ejecutar un movimiento
SigJugador :: Jugador -> Jugador
SigJugador X = O
SigJugador O = X

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


-- MODULOS ---------------------------------------------------------------------------
-- Implementacion del tablero de juego
module Tablero(
    --Atributos
    numCols, numFilas, 
    --METODOS
    --Creacion de un tablero vacio
    tableroVacio :: Tablero
    
    --Transformacion de posiciones de vectores a lista
    columnaLista :: Tablero -> Int -> [Posicion]
    filaLista :: Tablero -> Int -> [Posicion]
    diagUpLista ::  Tablero -> Int -> Int -> [Posicion]
    diagDownLista :: Tablero -> Int -> Int -> [Posicion]

    --Obtencion de la posicion dadas las coordenadas
    getPosicion :: Tablero -> Int -> Int -> Posicion

    --Poner ficha de un jugador en una columna
    ponerFicha :: Tablero -> Jugador -> Int -> Maybe Tablero

    movimientoValido :: Board -> Int -> Bool

    --Dice si existen 4 piezas del mismo jugador seguidas
    cuatroEnRaya :: Vec.Vector Posiciones -> Maybe Jugador


) where

    tableroVacio = Vec.replicate numCols $ Vec.replicate numFilas Nothing


-- Implementacion del estado de la partida
module Estado(
    --Atributos
    _tablero :: Tablero
    _turno :: Jugador
    _terminado :: Bool
    _ultimoMovimiento :: (Int, Int)
    --Metodos

) where


-- ESTRUCTURA PRINCIPAL DEL JUEGO ----------------------------------------------------
-- Llamada inicial
main :: IO ()
main = do
    main = do
    diff = chooseIA         --Seleccion de la IA
    let t = rand 0 1        --Generación del turno inicial
        if(t == 0) then turn = false
        else turn = true
    --Crear tablero vacio
    board <- tableroVacio
    --Llamar bucle de juego

-- Bucle principal del juego
play :: Estado -> Estado --No se que es exactamente lo de GameMonad...
play = do
    --Ha terminado la partida? Hay ganador?
    depende de PartidaTerminada
    --Leer entrada de datos del jugador (usuario o IA)
    Jugada <- leerJugada
    --Ejecutar movimiento
    ponerFicha
    jugador = SigJugador jactual
    --Comprobar si el movimiento es ganador
    do cuatroEnRaya . columnaLista
    do cuatroEnRaya . filaLista
    do cuatroEnRaya . diagUpLista
    do cuatroEnRaya . diagDownLista
    si alguna es valida -> PartidaTerminada = Ganador jugador
    si ninguna es valida -> PartidaTerminada = NoTerminada
    --Recursividad
    play