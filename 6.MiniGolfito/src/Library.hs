module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Punto 1
-- a
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 (((2*). precisionJugador) habilidad) 0

madera :: Palo
madera habilidad = UnTiro 100 (((div 2). precisionJugador) habilidad) 5

hierro :: Number -> Palo
hierro n habilidad = UnTiro (((*n) . fuerzaJugador) habilidad) (((/n). precisionJugador) habilidad) (max 0 (n-3))

-- b
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1 ... 10 ]

-- Punto 2
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo $ habilidad jugador

-- Punto 3
data Obstaculo = UnObstaculo {
    superaObstaculo :: Tiro -> Bool,
    efectoObstaculo :: Tiro -> Tiro
} deriving (Show)

noSuperaObstaculo :: Tiro
noSuperaObstaculo = UnTiro 0 0 0

-- a
tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo {superaObstaculo = superaTunelConRampita, efectoObstaculo = efectoTunelConRampita}  

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = ((>90) $ precision tiro) && (((==0) . altura) tiro)

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro 
    | superaTunelConRampita tiro = UnTiro {velocidad = 2 * velocidad tiro, precision = 100, altura = 0}
    | otherwise = noSuperaObstaculo

-- b
laguna :: Number -> Obstaculo
laguna largo = UnObstaculo {superaObstaculo = superaLaguna, efectoObstaculo = efectoLaguna largo }

superaLaguna :: Tiro -> Bool
superaLaguna tiro = ((>80) $ velocidad tiro) && (between 1 5 $ altura tiro)

efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiro 
    | superaLaguna tiro = tiro {altura = (div largo) $ altura tiro}
    | otherwise = noSuperaObstaculo

-- c
hoyo :: Obstaculo
hoyo = UnObstaculo {superaObstaculo = superaHoyo, efectoObstaculo = efectoHoyo}

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 $ velocidad tiro) && ((==0) $ altura tiro) && ((>95) $ precision tiro)

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = noSuperaObstaculo

-- Punto 4
-- a
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = foldr (\e a -> condicion e jugador obstaculo a) [] palos

condicion :: Palo -> Jugador -> Obstaculo -> [Palo] -> [Palo]
condicion palo jugador obstaculo listapalos 
    | ((superaObstaculo obstaculo) . palo . habilidad) jugador = [palo] ++ listapalos
    | otherwise = listapalos

-- b   COPIADO DE VIDEO
cantidadObstaculosSuperados :: Tiro -> [Obstaculo] -> Number
cantidadObstaculosSuperados tiro listaObstaculos = (length . takeWhile (\ (obstaculo, tiroLlega) -> superaObstaculo obstaculo tiroLlega) . zip listaObstaculos . tirosSucesivos tiro) listaObstaculos

tirosSucesivos :: Tiro -> [Obstaculo] -> [Tiro]
tirosSucesivos tiro = foldl (\a e -> a++[((efectoObstaculo e) . last) a] ) [tiro]

-- c
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador listaObstaculos = maximoSegun (flip cantidadObstaculosSuperados listaObstaculos . golpe jugador) palos

-- Punto 5
jugadorGolf = fst
puntosGolf = snd

listaPierden :: [(Jugador, Puntos)] -> [String]
listaPierden lista = (map (padre.jugadorGolf) . filter (== (buscarMaximoPuntos lista))) lista

buscarMaximoPuntos :: [(Jugador, Puntos)] -> (Jugador, Puntos)
buscarMaximoPuntos [a] = a
buscarMaximoPuntos (x:y:xs) 
    | mayorSegun puntosGolf x y == x = buscarMaximoPuntos (x:xs)
    | otherwise = buscarMaximoPuntos (y:xs)
