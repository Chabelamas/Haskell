module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Ley = Ley {
    tema :: String,
    presupuesto :: Number,
    gruposQueApoyan :: [String]
}deriving (Show, Eq)

usoMedicinalCannabis = Ley {tema = "Uso cannabis medicinalmente", presupuesto = 5, gruposQueApoyan = ["Partido Cambio de Todos", "Sector Financiero"]}
educacionSuperior = Ley {tema = "Educacion superior", presupuesto = 30, gruposQueApoyan = ["Docentes Universitarios", "Partido de Centro Federal"]}
profesionalizaciónTenisDeMesa = Ley {tema = "Profesionalización del tenista de mesa", presupuesto = 1, gruposQueApoyan = ["Partido de Centro Federal","Liga de Deportistas Autónomos","Club Paleta Veloz"]}
tenis = Ley {tema = "Tenis", presupuesto = 2, gruposQueApoyan = ["Liga de Deportistas Autónomos"]}

-- Punto 1
leyesCompatibles :: Ley -> Ley -> Bool
leyesCompatibles ley1 = foldr (\ grupo bool -> bool || (elem grupo $ gruposQueApoyan ley1)) False . gruposQueApoyan

-- Punto 2
type Juez = Ley -> Bool 

type CorteSuprema = [Juez]

type Agenda = [String]

agenda :: Agenda
agenda = ["Ping Pong", "Tenis"]

opinionPublica :: Juez
opinionPublica ley = elem (tema ley) agenda

arcasDelEstado :: Number -> Juez
arcasDelEstado numero = (<numero) . presupuesto

apoyoPartido :: Juez
apoyoPartido = elem ("Partido Conservador") . gruposQueApoyan

noApoyaSector :: Juez
noApoyaSector = not . elem ("Sector Financiero") . gruposQueApoyan

corteEjemplo :: CorteSuprema
corteEjemplo = [opinionPublica, arcasDelEstado 10, arcasDelEstado 20, apoyoPartido, noApoyaSector]

constitucionalidad :: Ley -> CorteSuprema -> Bool
constitucionalidad ley corteSuprema =  (< ((div 2) $ length corteSuprema)) $ foldr (\ e a -> a + votoAFavor ley e) 0 corteSuprema

votoAFavor ::  Ley -> Juez -> Number
votoAFavor ley juez
    | juez ley == True = 1
    | otherwise = 0

votSi :: Juez
votSi _ = True

dependeDeResultadoDeBoca :: Bool -> Juez
dependeDeResultadoDeBoca resultadoBoca _ = resultadoBoca

nuevosJueces :: CorteSuprema
nuevosJueces = [dependeDeResultadoDeBoca True , votSi , arcasDelEstado 5]

agregarNuevosJueces :: CorteSuprema -> CorteSuprema
agregarNuevosJueces corte = corte ++ nuevosJueces 

aprobadasNuevosIntegrantes :: [Ley] -> CorteSuprema -> CorteSuprema -> [Ley]
aprobadasNuevosIntegrantes grupoLeyes corteOG nuevosIntegrantes = filter (leyDenegadayLuegoAceptada corteOG (corteOG++nuevosIntegrantes)) grupoLeyes

leyDenegadayLuegoAceptada :: CorteSuprema -> CorteSuprema -> Ley -> Bool
leyDenegadayLuegoAceptada corteOG corteNueva ley = not $ constitucionalidad ley corteOG  && constitucionalidad ley corteNueva

-- Punto 3
borocotizar :: CorteSuprema -> CorteSuprema
borocotizar = map (not.)

-- Punto 4
coincideConSector :: Juez -> [Ley] ->Bool
coincideConSector juez  = (>0) . length . foldl1 (\l1 l2 -> filter (flip elem l1) l2) . map gruposQueApoyan . leyesAprobadas juez

leyesAprobadas :: Juez -> [Ley] -> [Ley]
leyesAprobadas juez conjutoLeyes = filter juez conjutoLeyes

-- Punto 5
{-
No, ya que el problema se romperia al tener infinitos casos que evaluar en las funciones de los jueces que involucran la participacion de un sector en particular. 
De esta manera, nunca podria llegar a evaluar cuantos casos a favor de la ley hay en total, al no contar con todos los votos analizados si son a favor o en contra.
Los jueces que podrian votar son los que no dependenden de los gruposQueApoyan : opinionPublica, arcasDelEstado, votSi, dependeDeResultadoDeBoca
-}