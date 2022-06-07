module Library where
import PdePreludat

-- Punto 1
data Producto = Producto {
    descripcion :: String,
    indicePeligrosidad :: Number, 
    componentes :: [String]
} deriving (Show,Eq)


nafta = Producto {descripcion = "nafta", indicePeligrosidad = 7, componentes = ["petroleo", "etanol"]}
bolitasNaftalina = Producto {descripcion = "bolitas de naftalina", indicePeligrosidad = 10, componentes = ["petroleo", "etanol", "sustancia blanca"]}
paradigmetileno = Producto {descripcion = "paradigmetileno", indicePeligrosidad = 2, componentes = ["funcionol", "logicol", "objetol"]}
escalonetina = Producto {descripcion = "escalonetina", indicePeligrosidad = 99, componentes = ["acido dibumartineico", "dimariol", "depaultinina", "liomessil"]}

buenaQuimica :: Producto -> Producto -> Bool
buenaQuimica prod1 prod2
    | foldr (\ e a -> ((a ||) . elem e . componentes) prod2) False (componentes prod1) && foldr (\ e a -> ((a ||) . elem e . descripcion) prod2) False (descripcion prod1)= True
    | foldr (\ e a -> ((a ||) . elem e . componentes) prod1) False (componentes prod2) && foldr (\ e a -> ((a ||) . elem e . descripcion) prod1) False (descripcion prod2)= True
    | otherwise = False

-- Punto 2
type Sensor = Producto -> Bool 
type Control = [Sensor]
type Listado = [String]

listaProdProhibidos :: Listado
listaProdProhibidos = ["paradigmetileno"]

--
ilegal :: Sensor
ilegal producto = elem (descripcion producto) listaProdProhibidos

contieneSustancia :: String -> Sensor
contieneSustancia sustancia = not. elem (sustancia) . componentes

peligrosidad :: Number -> Sensor
peligrosidad numero = (< numero) . indicePeligrosidad

listaSensores :: Control
listaSensores = [ilegal, contieneSustancia "petroleo", peligrosidad 50, peligrosidad 5, contieneSustancia "funcionol"]
--

habilitar :: Producto -> Control -> Bool
habilitar producto control =  ((>(div 2 . length) control) . foldr (\ e a -> aprobado e producto a) 0) control

aprobado :: Sensor -> Producto -> Number -> Number
aprobado sensor producto cuenta 
    | sensor producto = cuenta + 1
    | otherwise = cuenta

-- Punto 3
todoSi :: Sensor
todoSi _ = True

-- Si el numero de dia en el que se evalua el producto es par, lo habilita
inventado :: (Number, Number, Number) -> Sensor
inventado (a, _ , _) _ = even a 

multiple :: [Sensor] -> Sensor
multiple sensores producto = ((==0) . length . filter (== False) . map (\ s -> s producto)) sensores

listaAnadir = [todoSi, inventado (8,6,2022), multiple [todoSi,ilegal], peligrosidad 10]

agregarSensores :: Control -> Control
agregarSensores control = control ++ listaAnadir

-- Punto 4
ahoraAceptados :: [Producto] -> Control -> [Producto]
ahoraAceptados listaProd control = filter (\e -> not $ habilitar e control && (habilitar e . agregarSensores) control) listaProd

-- Punto 5
invertir :: Control -> Control
invertir = map (not.) 

-- Punto 6
-- Resuelto en Spec.hs
ejemplo = Producto {descripcion = "ejemplo", indicePeligrosidad = 100, componentes= ["funcionol", "petroleo"]}

-- Punto 7
{-
No puede ser habilitado debido a que hay funciones que requieren analizar los componentes que tiene un producto.
Por consiguinete, el programa se romperia al tener que analizar una lista infinita.
Sin embargo, si no se tienen en cuenta las funciones : contieneSustancia y multiple (en caso de que tenga en su lista de entrada de sensrores a la funcion anterior),
el programa podria habilitar al producto en cuestion. De esta manera, las funciones que se podrian tener en cuenta en este caso, para que se pueda habilitar el producto,
deberian ser: inventado, todoSi, peligrosidad, ilegal
-}