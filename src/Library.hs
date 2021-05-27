module Library where
import PdePreludat

-- Punto 1
-- Modelado del turista
-- Ana y beto de ejemplos

data Turista = UnTurista {
    nivelDeStress :: Number
,   nivelDeCansancio :: Number
,   viajaSolo :: Bool
,   idiomas :: [Idioma]
} deriving Show

type Idioma = String

-- Otra forma es definir un data Idioma = Español | ...

-- CFD : Coeficiente de Felicidad Docente
ana :: Turista
ana = UnTurista {
    nivelDeCansancio = 0
,   nivelDeStress = 21
,   viajaSolo = False
,   idiomas = ["español"]
}

beto :: Turista
beto = UnTurista {
    nivelDeCansancio = 15
,   nivelDeStress = 15
,   viajaSolo = True
,   idiomas = ["aleman"]  
}

-- Punto 2a
-- Hacer una excurison
-- Modelar las excursiones de ejemplo


-- Alternativas??
-- Podrian modelarlo con Datas, pero es mucho mas engorroso
-- Como solo las queremos para hacerlas, una funcion calza mejor
type Excursion = Turista -> Turista

-- Recibe una excursion y un turista, y devuelve el turista modificado
-- Con dos excursiones tipa pero no muestra lo que queremos
-- Es mas declarativa que hacerExcursion' porque esconde el algoritmo
-- Para hablar de declaratividad siempre compararmos con otra alternativa
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = reducirStressPorExcursion . excursion

reducirStressPorExcursion :: Turista -> Turista
reducirStressPorExcursion turista = modificarNivelDeStress (nivelDeStress turista * (-0.1)) turista

--hacerExcursion' excursion persona = modificarStressEn (-0.1 * stress (excursion persona)) (excursion persona)
-- La menos declarativa de todas: 
--hacerExcursion'' excursion turista = turista { nivelDeCansancio = (...), nivelDeStress = (...)}
-- TOP DOWN O BOTTOM UP (intentamos ir siempre TOP - DOWN)

modificarNivelDeStress :: Number -> Turista -> Turista
modificarNivelDeStress numero turista = turista {nivelDeStress = nivelDeStress turista + numero }

modificarNivelDeCansancio :: Number -> Turista -> Turista
modificarNivelDeCansancio numero unTurista = unTurista { nivelDeCansancio = nivelDeCansancio unTurista + numero }

-- Cuidado con el miedo a los Booleanos
irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista = modificarNivelDeCansancio (-5) turista -- (-5) es un numero
                   | otherwise = modificarNivelDeStress (-1) turista

-- Se podria poner el type y hacer un alias para length

-- + CFD con respecto a apreciar'
-- acá estoy operando funciones:
apreciar :: String -> Excursion
apreciar elemento = modificarNivelDeStress (- length elemento)

-- Esta vale tambien
-- Acá estoy operando datos:
apreciar' :: String -> Turista -> Turista
apreciar' elemento turista = modificarNivelDeStress (- length elemento) turista

-- Como la usariamos en hacerExcursion?
-- > hacerExcursion 


salirAHablar :: Idioma -> Excursion
salirAHablar idioma = ganarCompania . aprenderIdioma idioma

aprenderIdioma :: Idioma -> Turista -> Turista
aprenderIdioma idioma turista = turista { idiomas = idiomas turista ++ [idioma]}

ganarCompania :: Turista -> Turista
ganarCompania turista = turista {viajaSolo = False}

caminar :: Number -> Excursion
caminar minutos = caminata (intensidadCaminata minutos)

caminata :: Number -> Turista -> Turista
caminata intensidad = modificarNivelDeStress (-intensidad) . modificarNivelDeCansancio intensidad

intensidadCaminata :: Number -> Number
intensidadCaminata minutos = 1 * minutos / 4

-- Fail Fast
-- Si haces el tipo el IDE te avisa
-- Aumenta CFD si no es un String
data Marea = Fuerte | Moderada | Tranquila deriving Show

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte = modificarNivelDeCansancio 10 . modificarNivelDeStress 6
paseoEnBarco Moderada = id
paseoEnBarco Tranquila = salirAHablar "aleman" . apreciar "mar" . caminar 10

--Pattern matching: para definir para UN punto del dominio
-- Guardas: para definir un conjunto de puntos.
-- paseoEnBarco marea | marea == Fuerte ...
{-
modificarCansancio :: (Number -> Number -> Number) -> Number -> Turista -> Turista
modificarCansancio operador cantidad turista = turista {
    nivelDeCansancio = operador (nivelDeCansancio turista) cantidad
}
-}

-- Punto 2b
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2
{-

> deltaExcursionSegun stress ana irALaPlaya
-3

La pregunta significa "Cuánto cambió el stress de ana al ir a la playa"
La respuesta significa "bajó 3"
-} 

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun fIndice turista exc = deltaSegun fIndice (hacerExcursion exc turista) turista


-- Punto 2c i
-- Saber si una excursión es educativa para un turista
esEducativa :: Excursion -> Turista -> Bool
esEducativa exc turista = deltaExcursionSegun (length.idiomas) turista exc > 0


-- Punto 2c ii
-- Conocer las excursiones desestresantes para un turista

desestresantesPara :: Turista -> [Excursion] -> [Excursion]
desestresantesPara turista = filter (esDesestresantePara turista)

esDesestresantePara :: Turista -> Excursion -> Bool
esDesestresantePara turista excursion = deltaExcursionSegun nivelDeStress turista excursion <= (-3)


-- Punto 3a
-- Hacer que un turista haga un tour
-- Modelar los tours de ejemplo


-- Opción con Data:
-- Vale, ojo q faltan las excursiones
-- data Tour = Completo | LadoB {excursion :: Excursion} | IslaVecina {marea :: Marea}
-- hacerUnTour :: Tour -> Turista -> Turista
-- hacerUnTour Completo unTurista = tour Completo ( unTurista {nivelDeStress = modificarNivelDeStress unTurista 4}) 
-- hacerUnTour unTour unTurista = tour unTour ( unTurista {nivelDeStress = modificarNivelDeStress unTurista 3})

-- Opción con el tour que sea una función:
-- type Tour = Turista -> Turista
-- ¿Como saco la cantidad de excursiones si mi tour es una funcion?

type Tour = [Excursion]

hacerTour :: Tour -> Turista -> Turista
hacerTour tour = realizarExcursionesTour tour . pagar tour

pagar :: Tour -> Turista -> Turista
pagar tour = modificarNivelDeStress (length tour)


-- Se puede hacer con fold o recursion, pero foldl mata recursion
-- Utilizar foldl hace que tu solucion sea mas declarativa con respecto a la recursion
realizarExcursionesTour :: Tour -> Turista -> Turista
realizarExcursionesTour tour turista = foldl hacerExcursionFlipeada turista tour

hacerExcursionFlipeada :: Turista -> Excursion -> Turista
hacerExcursionFlipeada turista excursion = hacerExcursion excursion turista

-- realizarExcursionesTour tour turista = foldl (flip hacerExcursion) turista tour

completo :: Tour
completo = [caminar 20, apreciar "cascada", caminar 40, salirAHablar "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminata (2*60)]

-- Cuidado con las guardas antes de tiempo! Repite codigo

-- Articulo: 
islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, excursionIslaVecina marea, paseoEnBarco marea]

excursionIslaVecina :: Marea -> Excursion
excursionIslaVecina Fuerte = apreciar "lago"
excursionIslaVecina _ = irALaPlaya

-- Punto 3b
-- Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista
hayAlgunoConvincentePara :: Turista -> [Tour] -> Bool
hayAlgunoConvincentePara turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista tour = any (esConvincenteExcursion turista) tour

esConvincenteExcursion :: Turista -> Excursion -> Bool
esConvincenteExcursion turista excursion = esDesestresantePara turista excursion && quedaAcompaniado (hacerExcursion excursion turista)

quedaAcompaniado :: Turista -> Bool
quedaAcompaniado = not . viajaSolo

-- Punto 3c
-- Saber la efectividad de un tour para un conjunto de turistas
efectividad :: Tour -> [Turista] -> Number
efectividad tour = sum . map (espiritualidad tour) . filter (flip esConvincente tour) 

espiritualidad :: Tour -> Turista -> Number
espiritualidad tour turista = (-1) * deltaExcursionSegun nivelDeStress turista (hacerTour tour) + (-1) * deltaExcursionSegun nivelDeCansancio turista (hacerTour tour)

-- Punto 4
-- a
tourInfinito :: Tour
tourInfinito = repeat irALaPlaya

-- otra forma
infinitasPlayas = irALaPlaya : infinitasPlayas
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

{-

Punto 4: Implementar y contestar en modo de comentarios o pruebas por consola
a. Construir un tour donde se visiten infinitas playas.
b. ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
c. ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

-}