--1.1
esMultiploDeTres :: Int -> Bool
esMultiploDeTres num = mod num 3 == 0

--1.2
esMultiploDe :: Int -> Int -> Bool
esMultiploDe num1 num2 = mod num1 num2 == 0

--1.3
cubo num = num^3

--1.4
area base altura = base * altura

--1.5
esBisiesto año = (esMultiploDe año 400) || (esMultiploDe año 4 && (not.(esMultiploDe año)) 100 ) 

--1.6
celsiusToFahr c = (c*(9/5))+32

--1.7
fahrToCelsius f = (f-32)*(5/9)

--1.8
haceFrioF f = fahrToCelsius f < 8

--1.9
