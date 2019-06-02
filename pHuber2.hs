instance Show (a->b) where
    show a = "condicion"

--1
data Cliente = Cliente {
    nombreCliente :: String,
    lugarVive :: String
} deriving (Show)

data Chofer = Chofer {
    nombreChofer :: String,
    kmAuto :: Int,
    viajes :: [Viaje],
    condicionViaje :: Condicion
} deriving (Show)

data Viaje = Viaje {
    fecha :: (Int,Int,Int),
    cliente :: Cliente,
    costo :: Int
} deriving (Show)

type Condicion = Viaje -> Bool

--2
cualquierViaje :: Viaje -> Bool
cualquierViaje _ = True

costoMayorA200 :: Viaje -> Bool
costoMayorA200 = (>200).costo

nombreConMasLetrasQue :: Int -> Viaje -> Bool
nombreConMasLetrasQue n = (>n).length.nombreCliente.cliente

clienteNoVivaEn :: String -> Viaje -> Bool
clienteNoVivaEn lugar = (/=lugar).lugarVive.cliente

--3.a
clienteLucas = Cliente "Lucas" "Victoria"
--3.b
choferDaniel = Chofer "Daniel" 23500 [(Viaje (20,4,2017) clienteLucas 150)] (clienteNoVivaEn "Olivos")
--3.c
choferAlejandra = Chofer "Alejandra" 180000 [] (cualquierViaje)

--4
puedeTomarViaje :: Chofer -> Viaje -> Bool
puedeTomarViaje unChofer = condicionViaje unChofer

--5
liquidacion :: Chofer -> Int
liquidacion unChofer = foldl sumarLiquidacion 0 (viajes unChofer)

sumarLiquidacion :: Int -> Viaje -> Int
sumarLiquidacion liq unViaje = liq + (costo unViaje)

--6
realizarUnViaje :: Viaje -> [Chofer] -> Chofer
realizarUnViaje unViaje choferes = efectuarViaje unViaje (elegirChofer unViaje (filter (flip puedeTomarViaje unViaje) choferes))

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje unViaje unChofer = unChofer {viajes = viajes unChofer ++ [unViaje]}

elegirChofer :: Viaje -> [Chofer] -> Chofer
elegirChofer unViaje choferes = foldr menosViajes (head choferes) (tail choferes)

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes sChofer nChofer | (length.viajes) nChofer < (length.viajes) sChofer = nChofer
                            | otherwise = sChofer

--7.a
choferNitoInfiy = Chofer "NitoInfy" 70000 (repetirViaje (Viaje (11,3,2017) clienteLucas 50)) (nombreConMasLetrasQue 2)

repetirViaje viaje = viaje : repetirViaje viaje
--7.b
{- No se puede porque liquidacion va a seguir sumando los 50 del viaje infinito indefinidamente -}
--7.c
{- Si se podria porque la funcion puedeTomarViaje solo necesita la condicion y los datos del nuevo viaje para llegar a una determinacion,
no se realizan operaciones sobre la lista infinita -}

--8
gongNeng :: (Ord b) => b -> (b->Bool) -> (a->b) -> [a] -> b 
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3

-- arg1 b
-- arg2 (b->Bool)
-- arg3 (a->b)
-- arg4 [a]
