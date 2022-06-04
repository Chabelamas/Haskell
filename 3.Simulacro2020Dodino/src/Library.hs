module Library where
import PdePreludat

data Ladron = Ladron {
    nombreLadron        :: String,
    habilidades         :: [String],
    armas               :: [Arma]
} deriving (Show)

data Rehen = Rehen {
    nombreRehen         :: String,
    nivelDeComplot      :: Number,
    nivelDeMiedo        :: Number,
    planContraLadrones  :: [Plan]
} deriving (Show)
type Arma = Rehen -> Rehen
type Plan = Ladron -> Ladron

--Funcion generales
modificarComplot :: Number -> Rehen -> Rehen
modificarComplot numero rehen = rehen {nivelDeComplot = nivelDeComplot rehen + numero }

modificarMiedo :: Number -> Rehen -> Rehen
modificarMiedo numero rehen = rehen {nivelDeMiedo = nivelDeMiedo rehen + numero }

cambiarArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
cambiarArmas funcion ladron = ladron {armas= funcion (armas ladron)}

--Armas
pistola :: Number -> Arma
pistola calibre rehen= (modificarMiedo (((3*).length.nombreRehen) rehen).modificarComplot (-(5*calibre))) rehen

ametralladora :: Number -> Arma
ametralladora balas rehen= (modificarMiedo (balas) . modificarComplot (-(((/2).nivelDeComplot) rehen))) rehen

aplicarArmaMasIntimidante :: [Arma] -> Arma
aplicarArmaMasIntimidante [arma] rehen = arma rehen
aplicarArmaMasIntimidante (arma:armas) rehen 
    | nivelDeMiedo (arma rehen) > nivelDeMiedo (head armas $ rehen)  = arma rehen
    | otherwise = aplicarArmaMasIntimidante armas rehen

--Intimidar
rio = Ladron {nombreLadron = "Rio", habilidades = [], armas = []} 
berlin = Ladron {nombreLadron = "Berlin", habilidades = [], armas = []}

disparos :: Ladron -> Arma
disparos ladron = aplicarArmaMasIntimidante (armas ladron) 

hacerseElMalo :: Ladron -> Arma
hacerseElMalo ladron 
    |nombreLadron ladron == nombreLadron berlin = modificarMiedo ((length.concat.habilidades) berlin) 
    |nombreLadron ladron == nombreLadron rio = modificarComplot 20
    |otherwise = modificarMiedo 10

--Planes
atacarLadron :: Rehen -> Plan
atacarLadron rehenAmigo ladron = cambiarArmas (drop ((length.nombreRehen) rehenAmigo))  ladron

esconderse :: Plan
esconderse ladron =  cambiarArmas (drop (((/3).length.habilidades) ladron))  ladron

-- Punto 1
tokio = Ladron {nombreLadron = "Tokio", habilidades = ["trabajo psicológico", "entrar en moto"], armas = [ametralladora 30, pistola 9, pistola 9]}
profesor = Ladron {nombreLadron = "Profesor", habilidades = ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"], armas = []}
pablo = Rehen {nombreRehen = "Pablo", nivelDeComplot = 40, nivelDeMiedo = 30, planContraLadrones = [esconderse]}
arturito = Rehen {nombreRehen = "Arturito", nivelDeComplot = 70, nivelDeMiedo = 50, planContraLadrones = [esconderse, atacarLadron pablo]}

-- Punto 2
esInteligente :: Ladron -> Bool
esInteligente = (>2).length.habilidades

-- Punto 3
armaNueva :: Arma -> Ladron -> Ladron
armaNueva arma = cambiarArmas ((arma) :)

-- Punto 4
intimidarRehen :: Arma -> Rehen -> Rehen
intimidarRehen funcionIntimidar = funcionIntimidar  
 
-- Punto 5
calmarAguas :: Ladron -> [Rehen] -> [Rehen]
calmarAguas ladron = map (aplicarDisparosParaIntimidar (disparos ladron))

aplicarDisparosParaIntimidar :: Arma -> Rehen -> Rehen 
aplicarDisparosParaIntimidar arma rehen 
    | nivelDeComplot rehen > 60 = arma rehen
    | otherwise = rehen

-- Punto 6
puedeEscaparDePolicia :: Ladron -> Bool
puedeEscaparDePolicia ladron = any ((== "disfrazarse de").(take 14)) (habilidades ladron)

-- Punto 7
pintaMal :: [Ladron] -> [Rehen] -> Bool
pintaMal ladrones rehenes = (promedioLista $ map nivelDeComplot rehenes) > ((promedioLista $ map nivelDeMiedo rehenes) * ((length. map armas) ladrones))

promedioLista :: [Number] -> Number
promedioLista lista = ((/(length lista)).sum) lista

-- Punto 8 

rebelarseContraLadron :: [Rehen] -> Ladron -> Ladron
rebelarseContraLadron rehenes ladron =  foldr ($) (ladron) ((concat.map (planContraLadrones)) rehenesModificados)
    where rehenesModificados = map (modificarComplot (-10)) rehenes

-- Punto 9
planValencia :: [Rehen] -> [Ladron] -> Number
planValencia rehenes ladrones = ((* 1000000)  . length . map (armas)) (map (rebelarseContraLadron rehenes) metralletaParaTodes)
    where metralletaParaTodes = map (armaNueva (ametralladora 45)) ladrones 

-- Punto 10
--No se puede hacer Length de una lista infita, al final se hace antes de multiplicar por 1m, sí se puede agregar un arma nueva porque se agrega al comienzo

-- Punto 11
--No habría problema, gracias al lazy evaluation, ya que haskell solo evalua aquellos campos necesarios y en este caso, no se accede a la lista de habilidades  

-- Punto 12
funcion :: c -> (a -> [b]) -> (c -> a -> Bool) -> Number -> [a] -> Bool
funcion cond num lista str = (> str) . sum . map (length . num) . filter (lista cond)

-- num es una funcion | recibe un elemento de la lista que no sabemos | retor una lista porque se le aplica Length (a -> [b])
-- sum retorna un numero, entonces str debe ser un numero
-- lista es una funcion que retorna un booleano porque es la condicion del filter,  cond es el parametro de la funcion y recibe también elementos de una lista que se eliminó (cond -> a -> Bool)
-- lista de elementos genéricos [a] que fue eliminada 
-- la función en su totalidad retorna un booleano porque termina con una función que devuelve un booleano