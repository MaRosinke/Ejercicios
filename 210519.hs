data Dia = Dia {
    año :: Int,
    mes :: Int,
    dia :: Int
} deriving Eq

data Invitado = Invitado {
    nombre :: String,
    calendario :: [RestriccionParaJuntarse]
}

cantidadDeDiasEnMes :: Int -> Int -> Int
cantidadDeDiasEnMes 2 _ = 28
cantidadDeDiasEnMes m a | elem m [1,3,5,7,8,10,12] = 31
                        | otherwise = 30

--1.a

type RestriccionParaJuntarse = Dia -> Bool

--1.b

puedeJuntarse :: [Dia] -> Invitado -> [Dia]
puedeJuntarse dias invitado = foldr (\restriccion dias -> filter (not.restriccion) dias) dias (calendario invitado)

--2.a

tengoUnaCita :: Dia -> Dia -> Bool
--tengoUnaCita dCita dConsulta = año dCita == año dConsulta && mes dCita == mes dConsulta && dia dCita == dia dConsulta
tengoUnaCita dCita dConsulta = dCita == dConsulta

--2.b

esFeriado :: [Dia] -> Dia -> Bool
esFeriado feriados dia = elem dia feriados

--2.c

podriaIrIndeseable :: Invitado -> Dia -> Bool
podriaIrIndeseable indeseable dia = ((/=[]).(puedeJuntarse [dia])) indeseable

--2.d

esFinDeMes :: Dia -> Bool
esFinDeMes unDia = (<=5) $ (cantidadDeDiasEnMes (mes unDia)  (dia unDia) - dia unDia)

--2.e

uhJustoTengoTurnoConElDentista :: Dia -> Bool
uhJustoTengoTurnoConElDentista _ = True

andres = Invitado "Andres" [tengoUnaCita (Dia 3 6 2019),esFeriado [(Dia 25 5 2019),(Dia 9 7 2019)],podriaIrIndeseable pablo,esFinDeMes,uhJustoTengoTurnoConElDentista]
pablo = Invitado "Pablo" []

--3

agendarCita :: Invitado -> Dia -> Invitado
agendarCita unInvitado unDia = unInvitado {calendario = calendario unInvitado ++ [tengoUnaCita unDia]}

--4.a

determinarDiaReunior :: [Dia] -> [Invitado] -> Dia
determinarDiaReunior (d:ds) invitados = foldl (mejorDia invitados) d ds

mejorDia :: [Invitado] -> Dia -> Dia -> Dia
mejorDia invitados elDia unDia  | cantidadQuePuedeJuntarse elDia invitados >=  cantidadQuePuedeJuntarse unDia invitados = elDia
                                | otherwise = unDia

cantidadQuePuedeJuntarse :: Dia -> [Invitado] -> Int
cantidadQuePuedeJuntarse unDia invitados = length $ filter (\inv -> elem unDia (puedeJuntarse [unDia] inv)) invitados

--4.b

confirmarReunion :: [Dia] -> [Invitado] -> [Invitado]
confirmarReunion reunion invitados = map (\inv -> agendarCita inv (determinarDiaReunior reunion invitados)) invitados --mal falta verificar que pueda antes de agenadar

--5

maratonReuniones :: [[Dia]] -> [Invitado] -> [Invitado]
maratonReuniones reuniones invitados = foldr confirmarReunion invitados reuniones
