{-1) Definir la funcion esMultiploDeAlguno, que dado un numero y una lista de numeros, devuelve True
si el numero es multiplo de alguno de los numeros de la lista.
Main> esMultiplo de Alguno 15 [2,3,4]
True -}

esMultiploDeAlguno:: Integer -> [Integer] -> Bool
esMultiploDeAlguno numero lista = any ((0==).(mod numero)) lista

{- 1.a) Resolver la funcion find' que encuentra el primer elemento que cumple una condición. No se puede
resolver con recursividad. Si ningún elemento cumple la condición dejar que falle.
find':: (a -> Bool) -> [a] -> a     *Main> find' even [41..339] => 42 -}

find':: (a->Bool) -> [a] -> a
find' condicion lista = head $ (filter condicion lista)
--find' condicion lista = (head.(filter condicion)) lista

{- 1.b) Aprovechar la funcion find' para aplicarla a este dominio. -}

data Politico = Politico {
    proyectosPresentados::[String],
    sueldo:: Float,
    edad:: Int
} deriving Show

politicos = [Politico ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"] 20000 81,
             Politico ["tratar de reconquistar luchas sociales"] 10000 63,
             Politico ["tolerancia 100 para delitos"] 15500 49]

{- Queremos encontrar:
a) un político joven (menos de 50 años)
b) alguno que haya presentado mas de 3 proyectos Presentados
c) alguno que haya presentado algún proyecto que tenga más de 3 palabras
No hay que generar funciones, sino aprovechar find' y desde la consola resuelva los 3 requerimientos. -}

{- *Main> find' ((50>).edad) politicos
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500.0, edad = 49}

*Main> find' ((>3).length.proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000.0, edad = 81}

*Main> find' (\x -> any (>3) (map (length.words) (proyectosPresentados x))) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000.0, edad = 81}

*Main> find' ((any (>3)).(map (length.words)).proyectosPresentados ) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000.0, edad = 81} -}

{- 3)Definir la funcion promediosAlumnos, que dada una lista de alumnos devuelve una lista de tuplas
que tenga el alumno y el promedio(Consideramos la division entera para el promedio y usamos la funcion div). -}

type Nombre = String
type Notas = [Int]

data Persona = Alumno {
    nombre :: Nombre,
    notas :: Notas
} deriving Show

{- Main > promediosAlumnos [(Alumno "juan" [8,6]),(Alumno "maria" [7,9,4]), (Alumno "ana" [6,2,4])]
[("juan",7),("maria",6),("ana",4)] -}

promediosAlumnos::[Persona] -> [(Nombre,Int)]
promediosAlumnos alumnos = map promedio alumnos

promedio:: Persona -> (Nombre,Int)
promedio alumno = (nombre alumno,div (foldr (+) 0 (notas alumno)) ((length.notas) alumno))

{- 4) Definir la funcion promedioSinAplazos, que dada una lista de listas, devuelve la lista de los
promedios de cada lista elemento, excluyendo los que sean menores a 6 que no se cuentan.
Main>promedioSinAplazos [[8,6],[6,6,4]]
[7,6] -}

promedioSinAplazos::[[Int]] -> [Int]
promedioSinAplazos listaNotas = map promedioNotas (map (filter (>5)) listaNotas)

promedioNotas :: [Int] -> Int
promedioNotas notas = div (foldr (+) 0 notas) (length notas)

{- 5) Definir la funcion aprobó, que dado un alumno devuelve True si el alumno aproboó.
Aclaració: se dice que el alumno aprobo si todas sus notas son 6 o mas.
Main>aprobo (Alumno "manuel" [8,6,2,4])
False -}

aprobo:: Persona -> Bool
aprobo alumno = all (>=6) (notas alumno)

{- 6) Definir la funcion aprobaron, que dada una lista de alumnos,devuelve los nombres de los alumnos
que aprobaron.
Main>aprobaron [Alumno "manule" [8,6,2,4] , Alumno "elena" [7,9,4,5], Alumno "ana" [6,2,4,2], Alumno "pedro" [9,6,7,10]]
["pedro"] -}

aprobaron::[Persona]->[Nombre]
aprobaron alumnos = map nombre (filter aprobo alumnos)

{- 7) Definir la funcion productos que dado una lista de nombres de productos y una lista de precios,
devuelva una lista de tuplas
Main> productos ["melon", "zapallo", "palta"] [15,10,12,7]
[("melon",15),("zapallo",10), ("palta", 12)] -}

productos :: [String] -> [Int] -> [(String,Int)]
productos _ [] = []
productos [] _ = []
productos (x:xs) (y:ys) = [(x,y)] ++ productos xs ys 

--productos' :: [String] -> [Int] -> [(String,Int)]
productos' nombres precios = zip nombres precios
productos'' nombres precios = zipWith (\x y -> (x,y)) nombres precios