--1.a

data Pais = Pais {
  ingPerCapita :: Float,
  pobActSectorPublico :: Int,
  pobActSectorPrivado :: Int,
  recursosNaturales :: [String],
  deudaConFMI :: Float
} deriving Show

--1.b

namibia = Pais 4140 400000 650000 ["Mineria","Ecoturismo"] 50
mexico = Pais 3000 250000 1000000 ["Petroleo","Bosques"] 100

--2

prestarle :: Float -> Pais -> Pais
prestarle millones unPais = unPais {deudaConFMI = deudaConFMI unPais + millones * 1.5}

reducirSectorPublico :: Int -> Pais -> Pais
reducirSectorPublico puestosTrabajo unPais = unPais {pobActSectorPublico = pobActSectorPublico unPais - puestosTrabajo, ingPerCapita = reducirIPP puestosTrabajo unPais}

reducirIPP :: Int -> Pais -> Float
reducirIPP puestosTrabajo unPais  | puestosTrabajo > 100 = ingPerCapita unPais * 0.8
                                  | otherwise = ingPerCapita unPais * 0.85

entregarExplotacion :: String -> Pais -> Pais
entregarExplotacion recurso unPais = unPais {deudaConFMI = deudaConFMI unPais - 2,recursosNaturales = filter (/=recurso) (recursosNaturales unPais)}

blindaje :: Pais -> Pais
blindaje = ((reducirSectorPublico 500).(\up -> prestarle (((/2).(/1000000).pbi) up) up))

pbi :: Pais -> Float
pbi unPais = ingPerCapita unPais * fromIntegral (pobActSectorPrivado unPais + pobActSectorPublico unPais)

--3.a

receta :: [(Pais->Pais)]
receta = [reducirSectorPublico 100, prestarle 200,entregarExplotacion "Mineria"]

--3.b

aplicarReceta :: [(Pais->Pais)] -> Pais -> Pais
aplicarReceta receta unPais = foldr ($) unPais receta

--4.a

puedeZafar :: [Pais] -> [Pais]
puedeZafar = filter tienePetroleo

tienePetroleo :: Pais -> Bool
tienePetroleo = any (=="Petroleo").recursosNaturales

--4.b

deudaTotal :: [Pais] -> Float
deudaTotal = sum.(map deudaConFMI)

--4.c

--5

estaOrdenadoDePeorAMejor :: [(Pais->Pais)] -> Pais -> Bool
estaOrdenadoDePeorAMejor [] _ = True
estaOrdenadoDePeorAMejor (r1:[]) unPais = mejoro r1 unPais 
estaOrdenadoDePeorAMejor (r1:r2:rs) unPais = mejoro r2 (aplicarReceta [r1] unPais) && estaOrdenadoDePeorAMejor rs (aplicarReceta [r1,r2] unPais)

mejoro :: (Pais->Pais) -> Pais -> Bool
mejoro unaReceta unPais = pbi unPais <= pbi (aplicarReceta [unaReceta] unPais)

