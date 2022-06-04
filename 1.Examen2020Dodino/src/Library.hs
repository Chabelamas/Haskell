module Library where
import PdePreludat

--Punto 1
data Heroe = Heroe {
    epiteto                :: String,
    indicedereconocimiento :: Number,
    artefactos             :: [Artefacto],
    tareas                 :: [Tarea]
    } deriving (Show)
type Artefacto = (NombreArtefacto, Rareza)
type Rareza = Number
type NombreArtefacto = String

--Funciones Generales
cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto epitetoNuevo heroe = heroe {epiteto = epitetoNuevo}

anadirArtefacto :: Artefacto -> Heroe -> Heroe
anadirArtefacto artefactoNuevo heroe = heroe {artefactos = artefactos heroe ++ [artefactoNuevo]}

cambiarReconocimiento :: Number -> Heroe -> Heroe
cambiarReconocimiento reconocimientoACambiar heroe = heroe {indicedereconocimiento = indicedereconocimiento heroe + reconocimientoACambiar}

--Punto 2
pasarAHistoria :: Heroe -> Heroe
pasarAHistoria heroe 
    | indicedereconocimiento heroe > 1000 = cambiarEpiteto "El mítico" heroe 
    | indicedereconocimiento heroe > 500 = ((cambiarEpiteto "El magnifico").anadirArtefacto ("La lanza del Olimpo", 100)) heroe
    | indicedereconocimiento heroe > 100 = ((cambiarEpiteto "Hoplita").anadirArtefacto ("Xiphos", 50)) heroe
    | otherwise = id heroe  

--Punto 3
type Debilidad = Heroe -> Bool
type Tarea = Heroe -> Heroe
data Bestia = Bestia {
 nombre                    :: String,
 debilidad                 :: Debilidad
} deriving (Show)

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto =  (cambiarReconocimiento (snd artefacto)).anadirArtefacto artefacto

escalarElOlimpo :: Tarea
escalarElOlimpo = cambiarReconocimiento (500).anadirArtefacto("El relámpago de Zeus", 500).efectoOlimpo

efectoOlimpo :: Heroe -> Heroe 
efectoOlimpo heroe =  heroe {artefactos =  filter ((>1000).snd) $ map (\ (n, r) -> (n, 3*r)) (artefactos heroe)}

ayudarCruzarCalle :: Number -> Tarea
ayudarCruzarCalle cuadras heroe = cambiarEpiteto ("gros" ++ oEsParaTodos (cuadras)) heroe

oEsParaTodos :: Number -> String
oEsParaTodos 0 = ""
oEsParaTodos 1 = "o"
oEsParaTodos n = "o" ++ oEsParaTodos (n-1)


matarUnaBestia :: Bestia -> Tarea
matarUnaBestia bestia heroe  
    | debilidad bestia (heroe) = cambiarEpiteto ("El asesino de " ++ (nombre bestia)) heroe
    | otherwise = (descartarPrimerArtefacto . cambiarEpiteto ("El cobarde")) heroe

descartarPrimerArtefacto :: Tarea
descartarPrimerArtefacto heroe = heroe {artefactos = tail (artefactos heroe)}

--Punto 4
heracles= Heroe {epiteto= "Guardian del Olimpo", indicedereconocimiento= 700, artefactos= [("pistola", 1000)], tareas= [(matarUnaBestia leonNemea)]}

--Punto 5
epitetoMayorA20  :: Heroe -> Bool
epitetoMayorA20  = (>=20). length. epiteto

leonNemea = Bestia {nombre= "Leon de Nemea", debilidad= epitetoMayorA20 }

--Punto 6
hacerTarea :: Tarea -> Heroe -> Heroe
hacerTarea tarea heroe = tarea $ heroe {tareas= (tarea : tareas heroe)}  

--Punto 7
type Puesto= (Heroe, Heroe)
sumatoriaRareza :: Heroe -> Number
sumatoriaRareza heroe = sum $ map (snd) (artefactos heroe)

presumirLogros :: Heroe -> Heroe -> Puesto
presumirLogros heroe1 heroe2 
   | indicedereconocimiento heroe1 > indicedereconocimiento heroe2 = (heroe1, heroe2)
   | indicedereconocimiento heroe1 < indicedereconocimiento heroe2 = (heroe2, heroe1)
   | sumatoriaRareza heroe1 > sumatoriaRareza heroe2 = (heroe1, heroe2)
   | sumatoriaRareza heroe1 < sumatoriaRareza heroe2 = (heroe2, heroe1)
   | otherwise = presumirLogros (foldr (\ t h -> hacerTarea t h ) heroe1 (tareas heroe2)) (foldr (\ t h -> hacerTarea t h ) heroe2 (tareas heroe1) )

--Punto 8
pepe = Heroe {epiteto= "Guardian", indicedereconocimiento= 100, artefactos= [], tareas= []}
-- Los compararía infinitamente, por lo que da error

--Punto 9
type Labor = [Tarea]
ejecutarLabor :: Labor -> Heroe -> Heroe
ejecutarLabor labor heroe = foldr (\ tareita heroeito -> hacerTarea tareita heroeito ) heroe labor

--Punto 10
-- No se podria porque daria error en la terminal


