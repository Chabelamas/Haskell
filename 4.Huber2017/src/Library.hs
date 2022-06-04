module Library where
import PdePreludat
doble :: Number -> Number
doble x = 2 * x

-- Punto 1
data Chofer = Chofer {
    nombre :: String,
    kilometrajeAuto :: Number,
    viajesTomados :: [Viaje],
    condicionViaje :: Condicion
} deriving (Show)

data Viaje = Viaje {
    fecha :: (Number, Number, Number),
    clienteQueViaja :: Cliente,
    costo :: Number
} deriving (Show,Eq)

data Cliente = Cliente {
    nombreCliente :: String,
    lugarCliente :: String
} deriving (Show, Eq)

type Condicion = Viaje -> Bool

-- Punto 2
cualquierViaje :: Condicion
cualquierViaje _ = True

condicionCosto :: Condicion
condicionCosto = (>200) . costo

condicionNombre :: Number -> Condicion
condicionNombre numero = (> numero) . length . nombreCliente . clienteQueViaja 

condicionLugar :: String -> Condicion
condicionLugar lugarProhibido = (/= lugarProhibido) . lugarCliente . clienteQueViaja

-- Punto 3 
lucas = Cliente {nombreCliente = "Lucas", lugarCliente = "Victoria"}

daniel = Chofer {nombre = "Daniel", kilometrajeAuto = 23500, viajesTomados = [Viaje (20, 4, 2017) lucas 150], condicionViaje = condicionLugar "Olivos"}

alejandra = Chofer {nombre = "Alejandra", kilometrajeAuto = 180000, viajesTomados = [], condicionViaje = cualquierViaje}

-- Prueba
luis = Cliente {nombreCliente = "Luis", lugarCliente = "Olivos"}
viaje1 = Viaje (20, 4, 2017) luis 150

viaje2 = Viaje (20, 4, 2017) luis 150
jose = Chofer {nombre = "Jose", kilometrajeAuto = 180000, viajesTomados = [(Viaje (20, 4, 2017) lucas 150), (Viaje (20, 3, 2017) lucas 150)], condicionViaje = condicionCosto}

-- Punto 4
tomarViaje :: Viaje -> Chofer -> Bool
tomarViaje viaje chofer = (condicionViaje chofer) viaje

-- Punto 5
liquidacionChofer :: Chofer -> Number
liquidacionChofer = sum . (map costo) .  viajesTomados 

-- Punto 6
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje  = (efectuarViaje viaje) . (foldr1 menosViajes) . filter (tomarViaje viaje) 

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje chofer = chofer {viajesTomados = viaje : viajesTomados chofer}

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes ch1 ch2 
    | (length . viajesTomados) ch1 < (length . viajesTomados) ch2 = ch1
    | otherwise = ch2

-- Punto 7 
nitoInfy = Chofer {nombre = "Nito Infy", kilometrajeAuto = 70000, viajesTomados = repetirViaje (Viaje (11, 3, 2017) lucas 50), condicionViaje = condicionNombre 2}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

{-
    b. ¿Puede calcular la liquidación de Nito? Justifique.
    No se puede calcular ya que es una lista infinita de valores y el programa se rompe al no poder leer todos los valores

    c. ¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 
    Si, debido al lazy evaluation ya que Haskell unicamente evalua los paremetros necesarios. En este caso, solo evalua la condicion del chofer
-}

-- Punto 8
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3

{-
    a. [a] = como ultimo parametro entra una lista en donde se le va a comenzar aplicando el map y todas las demas funciones que se encuentras aplicadas con composicion. A traves del uso de point free, se elimina la lista de ambos lados del igual, es por ello que no se puede observar en la definicion

    b. arg3 = es una funcion que va de un tipo de dato a en otro, que en este caso es un dato c.  -- 3 es una función, no sé qué recibe, pero sé que retorna, lo que toma el filter (c) y recibe un elemento de la lista eliminada a (a->c)

    c. arg2 = va de un tipo de dato igual que el de salida de arg3 pero, en esta funcion, devuelve un Bool. Esto se puede deducir a partir de los parametro de la condicion del filter (tipo de dato -> Bool)

    d. arg1 = es del mismo tipo de dato que el de salida de arg3. Esto se debe a que es el tipo de dato de entrada del max y segun el tipo de funcion de max, los valores de entrada deben ser del mismo tipo que el de salida

    e. La salida de esta funcion debe ser del tipo c ya que, como se explico anteriormente, el max devuelve un tipo de dato igual que el de entrada
-}