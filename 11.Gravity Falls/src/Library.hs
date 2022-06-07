module Library where
import PdePreludat

apruebaParcial :: (String,String) -> Bool
apruebaParcial ("Chabe","Jere") = True
apruebaParcial ("Matos",_) = False
apruebaParcial _ = False

-- Primera parte: Las Rarezas
-- Punto 1
type Vulnerabilidad = Persona -> Bool

data Criatura = UnaCriatura {
    peligrosidad :: Number,
    vulnerabilidades :: [Vulnerabilidad]
} deriving (Show)

data Persona = UnaPersona {
    edad :: Number,
    items :: [String],
    experiencia :: Number
} deriving (Show)


siempredetras :: Criatura
siempredetras = UnaCriatura {peligrosidad = 0, vulnerabilidades = []}

gnomos :: Number -> Criatura
gnomos cantGnomos = UnaCriatura {peligrosidad = 2^cantGnomos, vulnerabilidades = [tieneItem "Soplador de Hojas"]}

fantasma :: Number -> [Vulnerabilidad] -> Criatura
fantasma nivelPoder vuln = UnaCriatura {peligrosidad = nivelPoder * 20, vulnerabilidades = vuln}


-- Punto 2
enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura persona criatura 
    | all ($ persona) (vulnerabilidades criatura) = adquirirExperiencia (peligrosidad criatura) persona
    | otherwise = adquirirExperiencia 1 persona

-- foldl (\bool vulne -> bool && vulne persona) True (vulnerabilidades criatura)

-- Punto 3
-- 3a
experienciaGanada :: Persona -> [Criatura] -> Number
experienciaGanada persona criaturas= (experiencia . foldl enfrentarCriatura persona) criaturas - (experiencia persona)


-- 3b
adquirirExperiencia :: Number -> Persona -> Persona
adquirirExperiencia xp persona = persona {experiencia = experiencia persona + xp}

menorEdad :: Number -> Vulnerabilidad
menorEdad edadLimite = (< edadLimite) . edad

tieneItem :: String -> Vulnerabilidad
tieneItem item = elem (item) . items 

experienciaMayorA :: Number -> Vulnerabilidad
experienciaMayorA numero = (> numero) . experiencia

criaturasEjemplo :: [Criatura]
criaturasEjemplo = [siempredetras,gnomos 10, fantasma 3 [menorEdad 13,tieneItem "disfrazDeOveja"],fantasma 1 [experienciaMayorA 10]]

-- Segunda parte: Mensajes ocultos
-- Punto 1
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf _ _ [] [] = []
zipWithIf _ _ [] (y:ys) = (y:ys)
zipWithIf _ _ _ [] = []
zipWithIf funcion condicion (x:xs) (y:ys) 
    | condicion y = (funcion x y) : zipWithIf funcion condicion xs ys
    | otherwise = y: zipWithIf funcion condicion (x:xs) ys

-- Punto 2
-- 2a
abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = init ([letra..'z'] ++ ['a'..letra])

abecedario = ['a'..'z']

-- 2b
desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraAbec letra
    | letraAbec > letra = (!!) (abecedario) (cuenta letraAbec letra)
    | otherwise = (!!) (abecedario) (length $ init [letraAbec..letra])

cuenta :: Char -> Char -> Number 
cuenta letraAbec letra = length (init ([letraAbec ..'z'])) + length ['a'..letra]

-- 2c
cesar :: Char -> String -> String
cesar letraClave  = zipWithIf (desencriptarLetra) (flip elem abecedario) (repeat letraClave)

-- 2d
consulta :: String -> [String]
consulta textoEncrip = map (flip cesar textoEncrip) abecedario

--Por consola : ["jrzel zrfaxal!","iqydk yqezwzk!","hpxcj xpdyvyj!","gowbi wocxuxi!","fnvah vnbwtwh!","emuzg umavsvg!","dltyf tlzuruf!","cksxe skytqte!","bjrwd rjxspsd!","aiqvc qiwrorc!","zhpub phvqnqb!","ygota ogupmpa!","xfnsz nftoloz!","wemry mesnkny!","vdlqx ldrmjmx!","uckpw kcqlilw!","tbjov jbpkhkv!","sainu iaojgju!","rzhmt hznifit!","qygls gymhehs!","pxfkr fxlgdgr!","owejq ewkfcfq!","nvdip dvjebep!","mucho cuidado!","ltbgn bthczcn!","ksafm asgbybm!"]

-- Punto 3  BONUS!!!
vigenere :: String -> String -> String
vigenere palabraClave = zipWithIf (desencriptarLetra) (flip elem abecedario) (cycle palabraClave) 