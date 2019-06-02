instance Show (a->b) where
    show a = "funcion"

data Elemento = UnElemento { 
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje)
} deriving  (Show)

data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
} deriving (Show)

--1.a
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio unPersonaje = unPersonaje {anioPresente = anio}

--1.b
meditar :: Personaje -> Personaje
meditar unPersonaje = editarSalud (*1.5) unPersonaje

editarSalud :: (Float->Float) -> Personaje -> Personaje
editarSalud f unPersonaje = unPersonaje {salud = f (salud unPersonaje)}
--1.c
causarDanio :: Float -> Personaje -> Personaje
causarDanio danio unPersonaje   | danio < (salud unPersonaje) = editarSalud (\s -> s - danio) unPersonaje
                                | otherwise = editarSalud (*0) unPersonaje

--2.a
esMalvado :: Personaje -> Bool
esMalvado = (any (esDeTipo "Maldad")).elementos

esDeTipo :: String -> Elemento -> Bool
esDeTipo unTipo = (==unTipo).tipo

--2.b
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento = salud unPersonaje - (salud $ (ataque unElemento) unPersonaje)

--2.c
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales unPersonaje enemigos = filter (tieneElementoMortal unPersonaje) enemigos

tieneElementoMortal :: Personaje -> Personaje -> Bool
tieneElementoMortal unPersonaje unEnemigo = any (esMortal unPersonaje) (elementos unEnemigo)

esMortal :: Personaje -> Elemento -> Bool
esMortal unPersonaje unElemento = danioQueProduce unPersonaje unElemento == salud unPersonaje

--3.a
concentracion :: Int -> Elemento
concentracion nivel = UnElemento "Magia" noIndicado (\unPer -> foldr ($) unPer (replicate nivel meditar))

noIndicado :: Personaje -> Personaje
noIndicado unPer = unPer

--3.b
esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cant = replicate cant (UnElemento "Maldad" (causarDanio 1) noIndicado)

--3.c
jack = UnPersonaje "Jack" 300 [concentracion 3,katanaMagica] 200

katanaMagica = UnElemento "Magia" (causarDanio 1000) noIndicado

--3.d
aku :: Int -> Float -> Personaje
aku anioAku saludAku = UnPersonaje "Aku" saludAku ([concentracion 4,portalAlFuturo anioAku] ++ esbirrosMalvados (100*anioAku)) anioAku

portalAlFuturo :: Int -> Elemento
portalAlFuturo anioAku = UnElemento "Magia" (mandarAlAnio (2800+anioAku)) (\unPer -> aku (2800+anioAku) (salud unPer))

--4
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar unPer otroPer    | salud otroPer == 0 = (unPer,otroPer)
                        | otherwise = luchar (ataca otroPer) (defiende unPer otroPer)

ataca :: Personaje -> Personaje
ataca unPersonaje = foldr (\elem per -> (defensa elem) per) unPersonaje (elementos unPersonaje)

defiende :: Personaje -> Personaje -> Personaje
defiende defensor atacante = foldr (\elem per -> (ataque elem) per) defensor (elementos atacante)

--5
f :: (Eq a,Num d) => (a -> b -> (c,c)) -> (d -> a) -> a -> [b] -> [c]
f x y z | y 0 == z = map (fst.x z)
        | otherwise = map (snd.x (y 0))

