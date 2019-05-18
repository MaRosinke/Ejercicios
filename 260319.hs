{- 1) Definir la funcion calcular, que recibe una tupla de 2 elementos, y devuelve
otra tupla segun las siguientes reglas:
> si el primer elemento es par lo duplica; si no lo deja como esta.
> si el segundo elemento es impar le suma 1; ai no swj como esta. -}

calcular :: (Integer,Integer) -> (Integer,Integer)
calcular (n1,n2)    | (even n1 && odd n2) = (n1*2,n2+1)
                    | (odd n1 && odd n2) = (n1,n2+1)
                    | (even n1 && even n2) = (n1*2,n2)
                    | otherwise = (n1,n2)

-- *Main> calcular (4,5)
-- (8,6)

{- 2) Definir las funciones booleanas estándar. Sin usar las funciones predefinidas.
2.1) Definir la función and'
2.2) Definir la función or' -}

and' :: Bool -> Bool -> Bool
and' p q    | p = q
            | otherwise = False

or' :: Bool -> Bool -> Bool
or' p q     | p = True
            | q = True
            | otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True q = q
and'' _ _ = False

or'' :: Bool -> Bool -> Bool
or'' True _ = True
or'' _ q = q

{- 3) Queremos calcular el sueldo de los empleados de nuestra empresa. Tenemos dos tipos de empleado:
> Los comunes: nos interesa saber el sueldo básico y el nombre.
> Los jerárquicos: nos interesa conocer el sueldo basico, la cantidad de gente a cargo y el nombre.
El sueldo que cobran los comunes se determina por el sueldo básico, en los empleados jerárquicos se
calcula como sueldo básico + plus por la cantidad de gente a cargo (500 por cada persona a cargo) -}

data Empleado = Comun {sueldoBasico :: Integer,nombre :: String} |
                Jefe {sueldoBasico :: Integer,genteACargo :: Integer,nombre :: String} deriving (Show)

sonia = Jefe {
    sueldoBasico = 15000,
    genteACargo = 3,
    nombre = "Sonia"
}

sueldo :: Empleado -> Integer
sueldo (Comun sueldo _) = (sueldo)
sueldo (Jefe sueldo gente _) = sueldo + (500 * gente)

{- 4) Dado un alumno así definido.
data Persona = Alumno Nombre Notas
type Nombre = String
type Notas = (Nota,Nota,Nota)
type Nota = Integer
4.1) Definir la función notaMaxima que dado un alumno devuelva la máxima nota del alumno.
(Nota resolverlo sin guardas)
4.2) Definir el tipo de la función notaMaxima usando tipos de datos propios. -}

data Persona = Alumno {
    nombre' :: Nombre,
    notas :: Notas
} deriving (Show)

type Nombre = String
type Notas = (Nota,Nota,Nota)
type Nota = Integer

notaMaxima :: Persona -> Nota
notaMaxima = (nMax.notas) 

nMax :: Notas -> Integer
nMax (n1,n2,n3) = max n3 (max n1 n2)

{- 5) Se conocen estas bebidas:
data Bebida = Cafe Nombre Azuxar | Gaseosa Sabor Azucar
Dado un producto determinar si es energizante.
> Si es café es energizante si es un capuchino.
> Si es una gaseosa es energizante si es de sabor pomelo y más de 10 gr de azúcar. -}

data Bebida = Cafe {nombre'' :: Nombre , azucar :: Azucar} | Gaseosa {sabor :: Sabor , azucar :: Azucar } deriving (Show)

type Azucar = Double
type Sabor = String

esEnergizante :: Bebida -> Bool
esEnergizante (Cafe "Capuchino" _) = True
esEnergizante (Cafe _ _) = False
esEnergizante (Gaseosa "Pomelo" azucar) = azucar > 10.0
esEnergizante (Gaseosa _ _) = False

{- 6) El doble del siguiente de la suma entre 3 y 6. -}

doble = (2 *)
siguiente = (1+)

dobleDelSiguienteDeLaSuma :: Int -> Int -> Int
dobleDelSiguienteDeLaSuma num1 = (doble.siguiente.(num1+))

{- 7) Dadas las siguientes funciones:
7.1) apply f x = f x. A que se reduce la siguiente expresion?
> apply fst (const 5 7, 4)
7.2) twice f x = f (f x). A que se reduce la siguiente expresion?
> twice (`div`2) 12 -}

apply f x = f x
twice f x = f (f x)

{- *
Main> apply fst (const 5 7, 4)
5
*Main> twice (`div` 2) 12
3 
-}