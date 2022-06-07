module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1
data Pizza = Pizza {
    ingredientes :: [String],
    tamano :: Number,
    cantidadCalorias :: Number
} deriving (Show, Eq)

grandeDeMuzza = Pizza {ingredientes = ["salsa","mozzarella","oregano"], tamano = 8, cantidadCalorias = 350}

-- Punto 2
nivelSatisfaccion :: Pizza -> Number
nivelSatisfaccion pizza 
    | (elem ("palmito") . ingredientes) pizza = 0
    | ((<500). cantidadCalorias) pizza = calculoSatisfaccion pizza
    | otherwise = ((div 2) . calculoSatisfaccion) pizza

calculoSatisfaccion :: Pizza -> Number
calculoSatisfaccion = (80*) . length . ingredientes 

-- Punto 3
valorPizza :: Pizza -> Number
valorPizza pizza =  ((* tamano pizza) . (* 120) . length . ingredientes) pizza

-- Punto 4
-- a
nuevoIngrediente :: String -> Pizza -> Pizza
nuevoIngrediente nuevoIngrediente = agregarCalorias (2 * length nuevoIngrediente) . agregarIngrediente nuevoIngrediente

agregarIngrediente :: String -> Pizza -> Pizza
agregarIngrediente nuevoIngrediente pizza = pizza {ingredientes = [nuevoIngrediente] ++ (ingredientes pizza)}

agregarCalorias :: Number -> Pizza -> Pizza
agregarCalorias numero pizza = pizza {cantidadCalorias = numero + cantidadCalorias pizza}

-- b
agrandar :: Pizza -> Pizza
agrandar pizza = pizza {tamano = min 10 $ tamano pizza + 2}

-- c
mezcladita :: Pizza -> Pizza -> Pizza
mezcladita pizza1 pizza2 = (agregarCalorias (((div 2) . cantidadCalorias) pizza1) . foldr (\e a -> condicion e a) pizza2 . ingredientes) pizza1 

condicion :: String -> Pizza -> Pizza
condicion e a 
    | any (== e) (ingredientes a) = a
    | otherwise = agregarIngrediente e a 

-- Punto 5
type Pedido = [Pizza]

nivelSatisfaccionPedido :: Pedido -> Number
nivelSatisfaccionPedido = sum . map (calculoSatisfaccion)

-- Punto 6
type Pizzeria = Pedido -> Pedido

-- a    
pizzeriaLosHijosDePato :: Pizzeria 
pizzeriaLosHijosDePato = map (agregarIngrediente "palmito")

-- b
pizzeriaElResumen :: Pizzeria
pizzeriaElResumen pedido = zipWith mezcladita pedido (tail pedido)

-- c
pizzeriaEspecial :: Pizza -> Pizzeria
pizzeriaEspecial predilectoPizza = map (mezcladita predilectoPizza)

pizzeriaPescadito :: Pizzeria 
pizzeriaPescadito = pizzeriaEspecial anchoasBasica

anchoasBasica = Pizza ["salsa", "anchoas"] 270 8

-- d
pizzeriaGourmet :: Number -> Pizzeria
pizzeriaGourmet nivelExquisitez = map (agrandar) . filter ((> nivelExquisitez) . calculoSatisfaccion)

pizzeriaLaJauja :: Pizzeria
pizzeriaLaJauja = pizzeriaGourmet 399

-- Punto 7
-- a
sonDignasDeCalleCorrientes :: Pedido -> [Pizzeria] -> [Pizzeria]
sonDignasDeCalleCorrientes pedido = filter (\x -> ((> nivelSatisfaccionPedido pedido) . nivelSatisfaccionPedido . x) pedido) 

--b
mejorPizzeria  :: Pedido -> [Pizzeria] -> Pizzeria
mejorPizzeria pedido = foldr1 (condicionMejorPizza pedido) 

condicionMejorPizza :: Pedido -> Pizzeria -> Pizzeria -> Pizzeria
condicionMejorPizza pedido pizzeria1 pizzeria2 
    | (nivelSatisfaccionPedido . pizzeria1) pedido > (nivelSatisfaccionPedido . pizzeria2) pedido = pizzeria1
    | otherwise = pizzeria2

-- Punto 8 
--                          1                   2           3         4
yoPidoCualquierPizza :: (a -> Number) -> (b -> Bool) -> [(a, b)] -> Bool
yoPidoCualquierPizza x y z = any (odd . x . fst) z && all (y . snd) z

-- 1 
-- x al estar compuesto, tiene que ser una funcion y como el tipo de dato de entrada de la funcion odd es Number, x tiene que ser una funcion que tenga de tipo de dato de salida , Number
-- Por otra parte, el tipo de dato de entrada de x debe ser el mismo que el de la primera componente de la tupla de z

-- 2
-- Debido a que la funcion all recibe como primer parametro una funcion de (a -> Bool), la y deberia ser una funcion que vaya del tipo de dato de la segunda componente de la tupla de z a un bool, para cumplir con el criterio

-- 3
-- z debe ser una lista al ser el ultimo parametro de entrada de la funcion any y all (tipo: (a -> Bool) -> [a] -> Bool)
-- Asimismo, z debe ser una lista de tuplas, ya que se realizan las funciones fst y snd sobre ellas (extraen el primer o segundo elemento de la tupla en cuestion)

-- 4
-- La funcion devuelve un Bool ya que, como se puede observar, hay un && donde evalua el valor de verdad de las funciones any y all (como mostre anteriormente, ambas funciones devuleven un Bool)

-- Punto 9
laPizzeriaPredilecta :: [Pizzeria] -> Pizzeria
laPizzeriaPredilecta = foldr1 (.) 