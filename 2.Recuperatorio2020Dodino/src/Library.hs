module Library where
import PdePreludat

data Serie = Serie {
    nombreSerie              :: String,
    actores             :: [Actor],
    presupuestoAnual    :: Number,
    cantidadTemporadas  :: Number,
    ratingPromedio      :: Number,
    cancelada           :: Bool
} deriving (Show, Eq)

data Actor = Actor {
    nombreActor         :: String,
    sueldo              :: Number,
    restricciones       :: [Restriccion]
} deriving (Show, Eq)

type Restriccion = Actor -> Bool
--Punto 1
enRojo :: Serie -> Bool
enRojo serie = ((presupuestoAnual serie >) . sum . map (sueldo)) (actores serie) 

esProblematica :: Serie -> Bool
esProblematica serie = ((>3). length . filter (>1) . map (length.restricciones)) (actores serie)

--Punto 2
type Productor = Serie -> Serie

--2a
conFavoritismos :: Actor -> Actor -> Productor
conFavoritismos actorFavorito1 actorFavorito2 = modificarActoresSerie (agregarActores actorFavorito1 actorFavorito2) . modificarActoresSerie (drop 2) 
    
agregarActores :: Actor -> Actor -> [Actor] -> [Actor]  
agregarActores actor1 actor2 listaActores =  [actor1] ++ [actor2] ++ listaActores
    
modificarActoresSerie :: ([Actor] -> [Actor]) -> Productor
modificarActoresSerie funcion serie = serie {actores = funcion (actores serie)} 

--2b
johnny = Actor {nombreActor= "Johnny Depp", sueldo = 20000000, restricciones = [] }
helena = Actor {nombreActor= "Helena Bonham Carter", sueldo = 15000000, restricciones = [] }

timBurton :: Productor
timBurton = conFavoritismos johnny helena

--2c
gatopardeitor :: Productor
gatopardeitor = id

--2d
estireitor :: Productor
estireitor serie = serie {cantidadTemporadas = ((2*).cantidadTemporadas) serie}

--2e
desespereitor :: Productor -> Productor -> Productor 
desespereitor productor1 productor2 = productor2 . productor1 

--2f
canceleitor :: Number -> Serie -> Serie 
canceleitor numero serie 
    | enRojo serie || ratingPromedio serie < numero = serie {cancelada = True}
    | otherwise = gatopardeitor serie

--Punto 3
bienestarSerie :: Serie -> Number
bienestarSerie serie 
    | cancelada serie = 0
    | otherwise = comparacionTemporadas serie + comparacionActores serie

comparacionTemporadas :: Serie -> Number 
comparacionTemporadas serie 
    | cantidadTemporadas serie > 4 = 5
    | otherwise = ((*2) . (-) 10) (cantidadTemporadas serie)

comparacionActores :: Serie -> Number 
comparacionActores serie
    | (length.actores) serie <10 = 3
    | otherwise = max 2 (10 - (length . filter (>1) . map (length.restricciones)) (actores serie))

--Punto 4
productorMasEfectivo :: [Serie] -> [Productor] -> [Serie]
productorMasEfectivo series productores = map (masEfectivo productores) series

masEfectivo :: [Productor] -> Serie -> Serie
masEfectivo (x:[]) serie = x serie 
masEfectivo (x:xs) serie
  | bienestarSerie (x serie) > bienestarSerie (head xs $ serie) = x serie
  | otherwise = masEfectivo xs serie

--Punto 5
-- a No es posible mostrarlo, pero si se hace dentro de otra operación lo realizaría
-- b sí puede porque los agrega al principio, pero se rompe si lo mostramos

--Punto 6
esControvertida :: Serie -> Bool
esControvertida serie = menorQueSiguiente (map (sueldo) (actores serie)) 

menorQueSiguiente :: [Number] -> Bool
menorQueSiguiente [] = True
menorQueSiguiente (x:y:ys) = x>y && (menorQueSiguiente (y:ys)) 

--Punto 7
funcionLoca :: (Number -> Number) -> (a -> [b]) -> [a] -> [Number]
funcionLoca x y = filter (even.x) . map (length.y) 

-- X (Number -> Number) devuelve enteros, porque está compuesto con even que recibe enteros | x recibe numeros porque el filter recibe la lista del map que devuelve numeros por el lenght
-- Y (a -> [b]) es una funcion que va de un tipo de dato a (lista eliminada), devolviendo una lista de otro tipo de dato
-- Falta una lista de elementos [a] que fue eliminada, porque el filter, map, etc, estám aplicados parcialmente
-- La funcionLoca retorna una lista de numeros debido a la caracteristica del filter que ingresa una lista de un tipo y devuelve una lista del mismo tipo (en este caso es Number por el lenght)