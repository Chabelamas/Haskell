module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Turista = Turista {
    nivelCansansio :: Number,
    nivelStress :: Number,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving (Show, Eq)

data Marea = Fuerte | Moderada | Tranquila

type Idioma = String
type Excursion = Turista -> Turista 
type Tour = [Excursion]

--
cambiarCansancio :: Number -> Turista -> Turista
cambiarCansancio numero turista = turista { nivelCansansio = ((+numero) . nivelCansansio) turista }

cambiarStress :: Number -> Turista -> Turista
cambiarStress numero turista = turista { nivelStress = ((+numero) . nivelStress) turista }

cambiarEstadoSoledad :: Turista -> Turista
cambiarEstadoSoledad turista = turista { viajaSolo = True }

sumarIdioma :: Idioma -> Turista -> Turista
sumarIdioma idioma turista = turista { idiomas = [idioma] ++ idiomas turista }

-- Excursiones 
irPlaya :: Excursion
irPlaya turista 
    | viajaSolo turista = cambiarCansancio (-5) turista
    | otherwise = cambiarCansancio (-1) turista

apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje elemento = cambiarStress (- (length elemento))

salirHablandoIdioma :: Idioma -> Excursion
salirHablandoIdioma idioma = cambiarEstadoSoledad . sumarIdioma idioma

caminarMinutos :: Number -> Excursion
caminarMinutos minutos = cambiarCansancio (div minutos 4) . cambiarStress (-(div minutos 4))

paseoBarco :: Marea -> Excursion
paseoBarco Fuerte = cambiarStress 6 . cambiarCansancio 10
paseoBarco Moderada = id
paseoBarco Tranquila = salirHablandoIdioma ("Aleman") . apreciarElementoPaisaje "mar" . caminarMinutos 10

-- Punto 1

ana = Turista 0 21 True ["Espanol"]
beto = Turista 15 15 False ["Aleman"]
cathi = Turista 15 15 False ["Aleman", "Catalan"]

-- Punto 2 
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = (cambiarStress (- (div (nivelStress turista) 10) ) . excursion) turista

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = (deltaSegun indice turista . excursion) turista

excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa turista  = (>0) . deltaExcursionSegun (length . idiomas) turista

excursionDesestresante :: Turista -> [Excursion] -> [Excursion]
excursionDesestresante turista = filter ((== 3) . deltaExcursionSegun nivelStress turista)

-- Punto 3
--CONSULTA POR QUE NO completo = Tour []
completo :: Tour
completo = [caminarMinutos 20, apreciarElementoPaisaje "cascada", caminarMinutos 40, irPlaya, salirHablandoIdioma "melmacquiano"]

ladoB :: Excursion -> Tour 
ladoB excursion = [paseoBarco Tranquila, excursion, caminarMinutos 120]

islaVecina :: Marea -> Tour 
islaVecina Fuerte = [paseoBarco Fuerte, apreciarElementoPaisaje "lago", paseoBarco Fuerte]
islaVecina marea = [paseoBarco marea, excursionMarea marea, paseoBarco marea]

excursionMarea :: Marea -> Excursion
excursionMarea Fuerte = apreciarElementoPaisaje "lago"
excursionMarea _ = irPlaya

{- SE PUEDE TAMBIEN ASI?
islaVecina :: Marea -> Tour 
islaVecina Fuerte = [paseoBarco Fuerte, apreciarElementoPaisaje "lago", paseoBarco Fuerte]
islaVecina marea = [paseoBarco marea, excursionMarea marea, paseoBarco marea]
-}

-- a)
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

-- b)
esConvincente :: Turista -> [Tour] -> Bool
esConvincente turista  = any (tourConvincente turista)

tourConvincente :: Turista -> Tour -> Bool
tourConvincente turista = any (dejaAcompaniado turista) . excursionDesestresante turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista

-- c)
efectividadTour :: Tour -> [Turista] -> Number
efectividadTour tour = foldl (\a e -> a + (deltaTourSegun nivelCansansio e tour) + (deltaTourSegun nivelStress e tour) ) 0 . filter (flip tourConvincente tour) 

deltaTourSegun :: (Turista -> Number) -> Turista -> Tour -> Number
deltaTourSegun indice turista tour = (deltaSegun indice turista . flip hacerTour tour) turista

-- Punto 4
-- a
repetirPlaya :: Tour
repetirPlaya = irPlaya : repetirPlaya

tourPlaya :: Tour
tourPlaya = repetirPlaya
--OPC 2: tourPlaya = repeat irPlaya

-- b
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}

-- c
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}