import Text.Show.Functions()
import Data.Char

main :: IO ()
main = return ()

type Gema = Personaje -> Personaje
type Universo = [Personaje]
type Habilidad = String

data Guantelete = CrearGuantelete {
                material :: String,
                gemas :: [Gema]
} deriving (Show)

data Personaje = CrearPersonaje {
                nombre :: String,
                edad :: Int,
                energia :: Int,
                habilidades :: [Habilidad],
                planeta :: String 
} deriving (Show)

guanteleteThanos :: Guantelete
guanteleteThanos = CrearGuantelete "uru" [] 

wolverine :: Personaje
wolverine = CrearPersonaje "Wolverine" 45 100 ["saltar","correr"] "Tuvieja"

capitanAmerica :: Personaje
capitanAmerica = CrearPersonaje "capitanAmerica" 45 100 [] "Tuvieja"

hulk :: Personaje
hulk = CrearPersonaje "hulk" 45 100 [] "Tuvieja"

estaCompletoElGuantelete :: Guantelete -> Bool
estaCompletoElGuantelete unGuantelete = (length.gemas) unGuantelete == 6

esMaterialUru :: Guantelete -> Bool
esMaterialUru unGuantelete = "uru" == material unGuantelete

chasquido :: Guantelete -> Universo -> Universo
chasquido unGuantelete unUniverso 
    | puedeChasquear unGuantelete = reducirUniverso unUniverso
    | otherwise = unUniverso 

puedeChasquear :: Guantelete -> Bool
puedeChasquear unGuantelete = estaCompletoElGuantelete unGuantelete && esMaterialUru unGuantelete

reducirUniverso :: Universo -> Universo
reducirUniverso unUniverso = take (div (length unUniverso) 2) unUniverso

aptoParaPendex :: Universo -> Bool
aptoParaPendex unUniverso = any (<45) (map edad unUniverso)

energiaTotal :: Universo -> Int
energiaTotal unUniverso = (sum.(map energia).habilidadesMayorA 1) unUniverso

habilidadesMayorA :: Int -> Universo -> Universo
habilidadesMayorA unValor unUniverso = filter ((>unValor).length.habilidades) unUniverso

cambiarEnergia :: Int -> Personaje -> Personaje
cambiarEnergia unValor unPersonaje = unPersonaje { energia= energia unPersonaje + unValor}

cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta unPlaneta unPersonaje = unPersonaje { planeta = unPlaneta} 

sacarHabilidad :: Habilidad -> Personaje -> Personaje
sacarHabilidad unaHabilidad unPersonaje = unPersonaje { habilidades = (filter (/=unaHabilidad).habilidades) unPersonaje }  

vaciarHabilidades :: Personaje -> Personaje
vaciarHabilidades unPersonaje = unPersonaje {habilidades = []}

esMenorEdad :: Int -> Bool
esMenorEdad unaEdad = unaEdad < 18

reducirEdad :: Personaje -> Personaje
reducirEdad unPersonaje 
    | (esMenorEdad.edad) unPersonaje = unPersonaje
    | esMenorEdad ((dividirEdad.edad) unPersonaje) = unPersonaje {edad = 18}
    | otherwise = unPersonaje {edad = (dividirEdad.edad) unPersonaje}

dividirEdad :: Int -> Int
dividirEdad unaEdad = div unaEdad 2

laMente :: Int -> Gema
laMente unValor unPersonaje = cambiarEnergia unValor unPersonaje

elAlma :: Habilidad -> Gema
elAlma unaHabilidad unOponente = (sacarHabilidad unaHabilidad.cambiarEnergia (-10)) unOponente

elEspacio :: String -> Gema
elEspacio unPlaneta unOponente = (cambiarEnergia (-20).cambiarPlaneta unPlaneta) unOponente

elPoder :: Gema
elPoder unOponente 
    | (length.habilidades) unOponente <=2 = (cambiarEnergia (-energia unOponente).vaciarHabilidades) unOponente
    | otherwise = cambiarEnergia (-energia unOponente) unOponente

elTiempo :: Gema
elTiempo unOponente = (reducirEdad.cambiarEnergia (-50)) unOponente

laGemaLoca :: Gema -> Gema
laGemaLoca unaGema = (unaGema.unaGema)