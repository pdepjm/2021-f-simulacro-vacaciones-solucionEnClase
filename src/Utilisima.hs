module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Objetivos:
-- 1. Marcar la cancha?
-- 2. Proponer una metodologia para resolver
--    el parcial.

-- Excursiones: Data vs Funcion
-- Data no porque no queremos preguntarle
-- nada a las excursiones, solo aplicarlas.

-- Punto 1
data Turista = UnTurista {
  nivelCansancio :: Number
, stress :: Number
, viajaSolo :: Bool
, idiomas :: [Idioma]
} deriving Show


type Idioma = String

ana :: Turista
ana = UnTurista {
    nivelCansancio = 0
,   stress = 21
,   viajaSolo = False
,   idiomas = ["español"]
}

beto :: Turista
beto = UnTurista {
    nivelCansancio = 15
,   stress = 15
,   viajaSolo = True
,   idiomas = ["aleman"]
}

cathi :: Turista
cathi = UnTurista {
    nivelCansancio = 15
,   stress = 15
,   viajaSolo = True
,   idiomas = ["aleman"]
}

alf :: Turista
alf = UnTurista {
    nivelCansancio = 100
,   stress = 200
,   viajaSolo = True
,   idiomas = ["español"]
}

-- Punto 2
-- a
-- Aca discusion de excursiones: Data vs Funcion

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = desestresarsePorcentual 10 . excursion 

hacerExcursion' excursion turista = desestresarsePorcentual 10 ( excursion turista)

desestresarsePorcentual :: Number -> Turista -> Turista
desestresarsePorcentual porcentaje turista = desestresarse (porcentaje / 100 * stress turista) turista

desestresarsePorcentual' porcentaje turista = turista{ stress = stress turista - (porcentaje / 100 * stress turista)}

desestresarse :: Number -> Turista -> Turista
desestresarse cantidad turista = turista { stress = stress turista - cantidad}

-- Excursiones
type Excursion = Turista -> Turista

apreciar :: ElementoPaisaje -> Excursion
apreciar elemento = desestresarse (cantLetras elemento)

irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista = descansar 5 turista
                   | otherwise = desestresarse 1 turista

descansar :: Number -> Turista -> Turista
descansar unidades turista = turista {nivelCansancio = nivelCansancio turista -  unidades}

-- overkill? Omitible por cuestiones de tiempo
type ElementoPaisaje = String

cantLetras :: ElementoPaisaje -> Number
cantLetras = length
--

salirAHablar :: Idioma -> Excursion
salirAHablar idioma = ganarCompania . aprenderIdioma idioma

aprenderIdioma :: Idioma -> Turista -> Turista
aprenderIdioma idioma turista = turista { idiomas = idiomas turista ++ [idioma]}

ganarCompania :: Turista -> Turista
ganarCompania turista = turista {viajaSolo = False}

caminar :: Number -> Excursion
caminar minutos = caminata (intensidadCaminata minutos)

caminata :: Number -> Turista -> Turista
caminata intensidad = desestresarse intensidad . aumentarCansancio intensidad

intensidadCaminata :: Number -> Number
intensidadCaminata minutos = 1 * minutos / 4

aumentarCansancio :: Number -> Turista -> Turista
aumentarCansancio cant turista = turista { nivelCansancio = nivelCansancio turista + cant }
-- Fail Fast
-- Si haces el tipo el IDE te avisa
data Marea = Fuerte | Moderada | Tranquila deriving Show

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte = aumentarCansancio 10 . desestresarse (-6)
paseoEnBarco Moderada = id
paseoEnBarco Tranquila = salirAHablar "aleman" . apreciar "mar" . caminar 10

estresarse :: Number -> Turista -> Turista
estresarse cantidad turista = turista { stress = stress turista + cantidad}

-- b
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

{-

> deltaExcursionSegun stress ana irALaPlaya
-3

La pregunta significa "Cuánto cambió el stress de ana al ir a la playa"
La respuesta significa "bajó 3"
-} 

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista 


-- c
esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun cantidadIdiomas turista excursion > 0

cantidadIdiomas :: Turista -> Number
cantidadIdiomas = length . idiomas

desestresantesPara :: Turista -> [Excursion] -> [Excursion]
desestresantesPara turista = filter (esDesestresantePara turista)

esDesestresantePara :: Turista -> Excursion -> Bool
esDesestresantePara turista excursion = deltaExcursionSegun stress turista excursion <= (-3)

-- Punto 3

-- F. No se puede modelar como funcion porque solo puedo aplicarla
-- Necesito saber la cantidad de excursiones
-- type Tour = Turista -> Turista

-- a

type Tour = [Excursion]

hacerUnTour :: Tour -> Turista -> Turista
hacerUnTour tour = hacerExcursiones tour . pagar tour

pagar :: Tour -> Turista -> Turista
pagar tour = estresarse (length tour)

hacerExcursiones :: Tour -> Turista -> Turista
hacerExcursiones tour turista = foldl (flip hacerExcursion) turista tour

hacerExcursiones' tour = foldl1 (\exc1 exc2 -> hacerExcursion exc1 . hacerExcursion exc2) tour

completo :: Tour
completo = [caminar 20, apreciar "cascada", caminar 40, salirAHablar "melmacquiano"]


ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminata (2*60)]

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, excursionIslaVecina marea, paseoEnBarco marea]

excursionIslaVecina :: Marea -> Excursion
excursionIslaVecina Fuerte = apreciar "lago"
excursionIslaVecina _ = irALaPlaya

-- Punto 3b)
hayAlgunoConvincentePara :: Turista -> [Tour] -> Bool
hayAlgunoConvincentePara turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista tour = any (esConvincenteExcursion turista) tour

esConvincenteExcursion :: Turista -> Excursion -> Bool
esConvincenteExcursion turista excursion = esDesestresantePara turista excursion && quedaAcompaniado (hacerExcursion excursion turista)

quedaAcompaniado :: Turista -> Bool
quedaAcompaniado = not . viajaSolo

-- Punto 3c)
efectividad :: Tour -> [Turista] -> Number
efectividad tour = sum . map (espiritualidad tour) . filter (flip esConvincente tour) 

espiritualidad :: Tour -> Turista -> Number
espiritualidad tour turista = (-1) * deltaExcursionSegun stress turista (hacerUnTour tour) + (-1) * deltaExcursionSegun nivelCansancio turista (hacerUnTour tour)

-- Punto 4
-- a
tourInfinito :: Tour
tourInfinito = repeat irALaPlaya

-- b  
-- Para saber si es convincente un tour, hay que saber si alguna excursión es convincente.
-- Entonces, hay que ver si irALaPlaya es convincente para Ana (que es una de las infinitas excursiones del tour).
-- Con el mismo ejemplo de deltaExcursionSegun del enunciado sabemos que irALaPlaya es desestresante, y además Ana está acompañada, así que existe una excursión convincente en el tour. 
-- Haskell no necesita evaluar la siguiente excursión de la lista infinita por Lazy Evaluation, ya devuelve True.

-- Por el contrario, para Beto irALaPlaya no es convincente, porque nunca pasa a estar acompañado. 
-- Pero Haskell no lo sabe esto, sigue construyendo y evaluando la lista infinita de excursiones y
-- jamás va a encontrar una excursión convincente. Se cuelga infinitamente por más que exista la Lazy Evaluation.

-- c
-- Mirando el código de efectividad:
-- Para conocer la efectividad del tour es necesario que el mismo se haga.
-- Esto no es posible porque hay que pagarlo y para eso necesito conocer la cantidad de excursiones que tiene el mismo
-- (y si la cantidad es infinita...).
