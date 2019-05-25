import Data.List

data Raton = CRaton {
    nombre::String,
    edad::Float,
    peso::Float,
    enfermedades::[String]
} deriving Show

cerebro = CRaton "Cerebro" 9.0 0.2 ["brucelosis","sarampion","tuberculosis"]

sufijosInfecciosas = ["sis","itis","emia","cocos"]

--1

modificarEdad :: (Float->Float) -> Raton -> Raton
modificarEdad fEdad raton = raton {edad = fEdad (edad raton)}

modificarNombre :: (String->String) -> Raton -> Raton
modificarNombre fNombre raton = raton {nombre = fNombre (nombre raton)}

modificarPeso :: (Float->Float) -> Raton -> Raton
modificarPeso fPeso raton = raton {peso = fPeso (peso raton)}

modificarEnfermedades :: ([String]->[String]) -> Raton -> Raton
modificarEnfermedades fEnfermedades raton = raton {enfermedades = fEnfermedades (enfermedades raton)}

--2

hierbaBuena :: Raton -> Raton
hierbaBuena raton = raton {edad = (sqrt.edad) raton}

hierbaVerde :: Raton -> String -> Raton
hierbaVerde raton sufijo = raton {enfermedades = filter (not.(coincideSufijo sufijo)) (enfermedades raton)}

coincideSufijo :: String -> String -> Bool
coincideSufijo sufijo enfermedad = elem sufijo (tails enfermedad)

alcachofa :: Raton -> Raton
alcachofa raton | (peso raton) > 2 = raton {peso = (peso raton)*0.9}
                | otherwise = raton {peso = (peso raton)*0.95}

hierbaZort :: Raton -> Raton
hierbaZort raton = ((modificarEdad (*0)).(\r -> foldl hierbaVerde r sufijosInfecciosas).(modificarNombre (const "Pinky"))) raton

--3
--a
medicamento :: [(Raton->Raton)] -> Raton -> Raton
medicamento listahierbas raton = foldr ($) raton listahierbas
--b
pondsAntiAge :: Raton -> Raton
pondsAntiAge raton = medicamento [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa] raton
--c
reduceFatFast :: Int -> Raton -> Raton
reduceFatFast pot raton = medicamento ([flip hierbaVerde "obesidad"] ++ (replicate pot alcachofa)) raton
--d
pdepCilina :: Raton -> Raton
pdepCilina raton = foldl hierbaVerde raton sufijosInfecciosas

--4
--a
cantidadIdeal :: (Int->Bool) -> Int
cantidadIdeal condicion = (head.(filter condicion)) [0..]
--b
estanMasLindosQueNunca :: [Raton] -> (Raton->Raton) -> Bool
estanMasLindosQueNunca ratones medicamento = all ((<1).peso) (map medicamento ratones)
--c
experimento :: [Raton] -> Int
experimento ratones = cantidadIdeal (\pot -> estanMasLindosQueNunca ratones (reduceFatFast pot)) 