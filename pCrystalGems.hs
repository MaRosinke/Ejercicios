instance Show (a->b) where
    show a = "reaccion"

aspectoP = UnAspecto "Incertidumbre" 100
aspectoP2 = UnAspecto "Tension" 50
aspectoP3 = UnAspecto "Peligro" 80

situacionP = [aspectoP,aspectoP2,aspectoP3]

data Aspecto = UnAspecto {
    tipoDeAspecto :: String,
    grado :: Float
} deriving (Show,Eq)

type Situacion = [Aspecto]

mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

--1.a
modificarAspecto :: (Float->Float) -> Aspecto -> Aspecto
modificarAspecto f aspecto = aspecto {grado = f (grado aspecto)}

--1.b
mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion mejor peor = foldr (compararSituacion peor) True mejor

compararSituacion :: Situacion -> Aspecto -> Bool -> Bool
compararSituacion unaSituacion unAspecto semilla = semilla && mejorAspecto unAspecto (buscarAspecto unAspecto unaSituacion)

--1.c
modificarSituacion :: (Float->Float) -> String -> Situacion -> Situacion
modificarSituacion f tipoAspecto unaSituacion = reemplazarAspecto (modificarAspecto f (buscarAspectoDeTipo tipoAspecto unaSituacion)) unaSituacion

--2.a
data Gema = UnaGema {
    nombre :: String,
    fuerza :: Float,
    personalidad :: Reaccion
} deriving (Show)

type Reaccion = Situacion -> Situacion

--2.b
vidente :: Reaccion
vidente = (modificarSituacion (flip (-) 10) "Tension").(modificarSituacion (/2) "Incertidumbre")

relajada :: Float -> Reaccion
relajada n = (modificarSituacion (+n) "Peligro").(modificarSituacion (flip (-) 30) "Tension")

--2.c
ruby = UnaGema "Ruby" 100 vidente
diamante = UnaGema "Diamante" 200 (relajada 0)

--3
ganaGema :: Gema -> Gema -> Situacion -> Bool
ganaGema gGana gPierde unaSituacion = fuerza gGana >= fuerza gPierde && mejorSituacion ((personalidad gGana) unaSituacion) ((personalidad gPierde) unaSituacion)

--4
fusion :: Gema -> Gema -> Situacion -> Gema
fusion unaGema otraGema unaSituacion = UnaGema (definirNombre unaGema otraGema) (definirFuerza unaGema otraGema unaSituacion) (definirPersonalidad unaGema otraGema)

definirNombre :: Gema -> Gema -> String
definirNombre unaGema otraGema  | nombre unaGema == nombre otraGema = nombre unaGema
                                | otherwise = nombre unaGema ++ nombre otraGema
        
definirPersonalidad :: Gema -> Gema -> Reaccion
definirPersonalidad unaGema otraGema = (personalidad otraGema).(personalidad unaGema).(map (modificarAspecto (flip (-) 10)))

definirFuerza :: Gema -> Gema -> Situacion -> Float
definirFuerza unaGema otraGema unaSituacion | sonCompatibles unaGema otraGema unaSituacion = (*10).(+(fuerza unaGema)) $ fuerza otraGema
                                            | ganaGema unaGema otraGema unaSituacion = (*7).fuerza $ unaGema
                                            | otherwise = (*7).fuerza $ otraGema

sonCompatibles :: Gema -> Gema -> Situacion -> Bool
sonCompatibles unaGema otraGema unaSituacion = mejorSituacion (definirPersonalidad unaGema otraGema unaSituacion) (personalidad unaGema unaSituacion) && mejorSituacion (definirPersonalidad unaGema otraGema unaSituacion) (personalidad otraGema unaSituacion)

--5
fusionGrupal :: [Gema] -> Situacion -> Gema
fusionGrupal gemas unaSituacion = foldl (\fGem nGem -> fusion fGem nGem unaSituacion) (head gemas) (tail gemas)

--6
foo :: (Foldable t1,Eq b) => c -> (c->b) -> (a->t1 b) -> a -> Bool
foo x y z = any (== y x).z

--foo x y z = any (== (+7) 5).[1..]
--foo x y z = any (== even 3).(map (<7))
--foo x y z = any (== even 3).[1,2,3]
--foo x y z = any (== head [1..]).(take 5) [1..]
