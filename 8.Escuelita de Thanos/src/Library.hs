module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1
data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show, Eq)

data Personaje = Personaje {
    nombre :: String,
    edad :: Number,
    planeta :: String,
    energia :: Number,
    habilidades :: [Habilidad]
} deriving (Show, Eq)

type Habilidad = String
type Gema = Personaje -> Personaje
type Universo = [Personaje]

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universo 
    | ((== "uru") $ material guantelete) && (((==6) . length . gemas) guantelete) = take ((div 2) $ length universo) universo
    | otherwise = universo

-- Punto 2    
aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((>45) . edad)

energiaTotal :: Universo -> Number
energiaTotal = sum . map energia . filter ((>1) . length . habilidades)

-- Punto 3
-- a
mente :: Number -> Gema 
mente valorAlterar = disminuirEnergia (valorAlterar)

disminuirEnergia :: Number -> Personaje -> Personaje
disminuirEnergia valor personaje = personaje {energia = energia personaje - valor} 

-- b
alma :: Habilidad -> Gema 
alma habilidad = disminuirEnergia (10) . eliminarHabilidad habilidad

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad habilidad personaje 
    | any (== habilidad) $ habilidades personaje = personaje {habilidades = [habilidad] ++ habilidades personaje}
    | otherwise = personaje

-- c
espacio :: String -> Gema
espacio planetaNuevo = disminuirEnergia 20 . modificarPlaneta planetaNuevo

modificarPlaneta :: String -> Personaje -> Personaje
modificarPlaneta planetaNuevo personaje = personaje {planeta = planetaNuevo}

-- d
poder :: Gema
poder enemigo = atacarHabilidades.disminuirEnergia (energia enemigo) $ enemigo

atacarHabilidades :: Personaje -> Personaje
atacarHabilidades personaje 
    | (<=2).length.habilidades $ personaje = foldr eliminarHabilidad personaje $ habilidades personaje
    | otherwise = personaje   

-- e
tiempo :: Gema
tiempo = modificarEdad . disminuirEnergia 50

modificarEdad :: Personaje -> Personaje
modificarEdad personaje = personaje {edad = ((max 18) . (div 2) . edad) personaje }

-- f
loca :: Gema -> Gema
loca gema = gema . gema

-- Punto 4
guanteleteEjemplo = Guantelete {material = "goma", gemas = [tiempo, alma "usar Mjolnir", loca (alma "programación en Haskell")]}

-- Punto 5
utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas enemigo = foldr ($) enemigo listaGemas  
-- el efecto es de derecha a izquierda por el foldr

-- Punto 6
gemaMasPoderosa :: Personaje -> [Gema] -> Gema
gemaMasPoderosa _ [gema] = gema
gemaMasPoderosa personaje (x:y:xs)
    | ((energia . x) personaje) > ((energia . y) personaje) = gemaMasPoderosa personaje (x:xs)
    | otherwise = gemaMasPoderosa personaje (y:xs)

-- Punto 7
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

{-
a)  gemaMasPoderosa guanteleteEjemplo guanteleteDeLocos
    No se puede realizar esta funcion ya que el programa se romperia al tener infinitos elementos a analizar (gemas)

b)  usoLasTresPrimerasGemas guanteleteDeLocos guanteleteEjemplo
    Si se podria realizar por la lazy evaluation que posee Haskell. En otras palabras, el prgrama solo analizaria las tres primeras gemas del guante. De esta manera, el programa no se romperia como en el punto a 
-}