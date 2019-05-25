--1

cantidadElementos::[(Int,Int)]->Int
cantidadElementos lista = foldr masUno 0 lista

cantidadElementos'::[(Int,Int)]->Int
cantidadElementos' lista = foldl (flip masUno) 0 lista

masUno::(Int,Int)->Int->Int
masUno tupla cont = cont + 1

--2

masGastador::[([Char],Integer)] -> ([Char],Integer)
masGastador (x:xs) = foldr compararGasto x xs

compararGasto:: ([Char],Integer) -> ([Char],Integer) -> ([Char],Integer)
compararGasto (nNue,gNue) (nVie,gVie)   | gNue >= gVie = (nNue,gNue)
                                        | otherwise = (nVie,gVie)

pruebag = [("juan",100),("lu",80),("walter",150),("adri",200)]
pruebag' = [("juan",1000),("lu",80),("walter",150),("adri",200)]
pruebag'' = [("juan",100),("lu",800),("walter",150),("adri",200)]
pruebag''' = [("juan",100),("lu",80),("walter",1500),("adri",200)]

--3

monto::[([Char],Integer)]->Integer
monto lista = foldl (\sem (n,g) -> sem + g) 0 lista

--4

data Flor = Flor {
    nombre::String,
    aplicacion::String,
    cantidadDeDemanda::Int
} deriving Show

rosa =Flor "rosa" "decorativo" 3
jazmin =Flor "jazmin" "aromatizante" 100
violeta =Flor "violeta" "infusion" 1100
orquidea =Flor "orquidea" "decorativo" 150

flores = [rosa,jazmin,violeta,orquidea]

--4.1

{- maximoSegun :: (Flor -> Flor -> Flor) -> [Flor] ->  Flor
maximoSegun condicion flores = foldl condicion (head flores) (tail flores) -}

cantidadDemandada :: Flor -> Flor -> Flor
cantidadDemandada florV florN   | cantidadDeDemanda florN >= cantidadDeDemanda florV = florN
                                | otherwise = florV

cantidadLetrasFlor :: Flor -> Flor -> Flor
cantidadLetrasFlor florV florN  | (length.nombre) florN >= (length.nombre) florV = florN
                                | otherwise = florV
restoDemandaDiv4 :: Flor -> Flor -> Flor
restoDemandaDiv4 florV florN    | (rem (cantidadDeDemanda florN) 4) >= (rem (cantidadDeDemanda florV) 4) = florN
                                | otherwise = florV


maximoSegun :: (Flor -> Flor -> Flor) -> [Flor] ->  Flor
maximoSegun _ (f:[]) = f
maximoSegun condicion (f:fs) = condicion f (maximoSegun condicion fs)

--5.a

pruebac = ["lechuga","anana","alfajor de chocolate","fideos","alfalfa","aaaed"]

f1::[String] -> [(String,(Int,Int))]
f1 comidas = filter masConsonantes (map cantCyV comidas)

cantCyV :: String -> (String,(Int,Int))
cantCyV comida = (comida,foldr contarLetras (0,0) comida)

contarLetras :: Char -> (Int,Int) -> (Int,Int)
contarLetras letra (c,v)    |elem letra ['a','e','i','o','u'] = (c,v+1)
                            |letra == ' ' = (c,v)
                            |otherwise = (c+1,v)

masConsonantes :: (String,(Int,Int)) -> Bool
masConsonantes (_,(c,v)) = c >= v

--5.b

data Persona = Persona {
    nombreP::String,
    lugar::String,
    fechaN::(Int,Int,Int)
} deriving Show

f2 :: [Persona] -> [(String,Int)]
f2 personas = filter esMenorDeEdad (map nombreYEdad personas)

nombreYEdad :: Persona -> (String,Int)
nombreYEdad persona = (nombreP persona,(\(d,m,a) -> 2019 - a) (fechaN persona))

esMenorDeEdad :: (String,Int) -> Bool
esMenorDeEdad (_,edad) = edad < 18

pruebap = [Persona "Luis" "Cordoba" (20,2,2005),Persona "Pedro" "Rosario" (5,5,1940),Persona "Marta" "Mendoza" (12,10,2003)]

--5.c

f3 :: [a] -> (a->b) -> (b->Bool) -> [b]
f3 lista mapeo filtro = filter filtro (map mapeo lista)

