instance Show (a->b) where 
    show a= "asunto"

--Primera Parte
--1

data Persona = Persona {
    edad::Int,
    items::[Item],
    experiencia::Int
} deriving (Show)

type Item = String
type Condicion = Persona->Bool

data Criatura =     Siempredetras 
                |   Gnomos{
                        cantGnomos::Int
                }|  Fantasma {
                        categoriaFantasma::Int,
                        asuntoPendiente::(String,[Condicion])
                } deriving (Show)

peligrosidad :: Criatura -> Int
peligrosidad (Siempredetras) = 0
peligrosidad (Gnomos cant) = 2^cant
peligrosidad (Fantasma cat _) = cat*20

--2
enfrentamiento :: Persona -> Criatura -> Persona
enfrentamiento unaPersona unaCriatura   | leGana unaPersona unaCriatura = darExperiencia unaPersona unaCriatura
                                        | otherwise = unaPersona {experiencia = experiencia unaPersona + 1}

leGana :: Persona -> Criatura -> Bool
leGana _ (Siempredetras) = False
leGana unaPersona (Gnomos _) = tieneItem "Soplador de hojas" unaPersona
leGana unaPersona (Fantasma _ (_,asuntos)) = all ($ unaPersona) asuntos

tieneItem :: String -> Persona -> Bool
tieneItem item unaPersona = elem item $ items unaPersona

darExperiencia :: Persona -> Criatura -> Persona
darExperiencia unaPersona unaCriatura = unaPersona {experiencia = experiencia unaPersona + peligrosidad unaCriatura}

--3.a

determinarExperiencia::Persona -> [Criatura] -> Int
determinarExperiencia unaPersona  = experiencia.(foldl enfrentamiento unaPersona) 

{- asunto1::Condicion
asunto1 unaPersona = ((<13).edad) unaPersona && tieneItem "Disfraz de oveja" unaPersona

asunto2::Condicion
asunto2 unaPersona = -}

gnomos = Gnomos 10
fantasma1 = Fantasma 3 ("asunto1",[(<13).edad,tieneItem "Disfraz de oveja"])
fantasma2 = Fantasma 1 ("asunto2",[(>10).experiencia])


personaPrueba = Persona 10 ["Soplador de hojas","Disfraz de oveja","Cuchillo"] 0
criaturasPrueba = [Siempredetras,gnomos,fantasma1,fantasma2]

--Segunda Parte
--1
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf f condicion _ [] = [] 
zipWithIf f condicion [] _ = [] 
zipWithIf f condicion (a:as) (b:bs)     | (not.condicion) b = [b] ++ zipWithIf f condicion (a:as) bs
                                        | otherwise = [f a b] ++ zipWithIf f condicion as bs

--2.a
abecedario = ['a'..'z']

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = filter (>=letra) abecedario ++ filter (<letra) abecedario
--abecedarioDesde letra = [letra..'z'] ++ ['a'..letra]

--2.b
desencriptarLetra :: Char -> Char -> Char
desencriptarLetra clave letra = snd.head.(filter ((==letra).fst)) $ zip (abecedarioDesde clave) abecedario

--2.c
cesar :: Char -> String -> String
cesar clave palabra = zipWithIf desencriptarLetra (\l->elem l abecedario) (repeat clave) palabra

--2.d
consulta = foldr (\letra lista -> lista ++ [cesar letra "jrzel zrfaxal!"]) [] abecedario

--BONUS
vigenere::String->String->String
vigenere clave palabraEnc =reverse $ foldr (armarPalabra) [] (asociarPalabraAClave palabraEnc (listaClave clave))

asociarPalabraAClave :: String -> String -> [(Char,Char)]
asociarPalabraAClave [] _ = []
asociarPalabraAClave palabra clave      | elem (head palabra) abecedario = [(head palabra,head clave)] ++ asociarPalabraAClave (tail palabra) (tail clave)
                                        | otherwise = [(head palabra,' ')] ++asociarPalabraAClave (tail palabra) clave

armarPalabra :: (Char,Char) -> [Char] -> [Char]
armarPalabra (letra,' ') palabra = palabra ++ [letra]
armarPalabra (letra,clave) palabra = palabra ++ [desencriptarLetra clave letra]

listaClave :: String -> String
listaClave clave = foldl (++) [] [clave] ++ listaClave clave