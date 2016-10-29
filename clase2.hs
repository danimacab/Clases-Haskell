-- Clase 02 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- TIPOS Y CLASES DE TIPOS --

f :: Bool -> Bool
f x = not x
-- not simplemente niega al booleano

f2 :: Bool -> Float
f2 x = pi
-- ni puta idea de qué se supone que hace esto
-- das un booleano y te devuelve 3,1416blablabla

f3 :: Integer -> Integer -> Bool -> Bool
f3 x y b = b || (x>y)
-- "||" es un O lógico

doble :: Integer -> Integer
doble x = x+x

cuadruple :: Integer -> Integer
cuadruple x = doble (doble x)

esPar :: Integer -> Bool
esPar x |mod x 2 == 0 = True
        |mod x 2 /= 0 = False

esPositivo :: Float -> Bool
esPositivo x |x > 0     = True
             |otherwise = False

identidad :: a -> a
identidad x = x 

crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a)
invertir p = (snd p,fst p)

dist :: (Float,Float) -> (Float,Float) -> Float
dist p q = sqrt((fst p - fst q)^2 + (snd p - snd q)^2)

raices :: Float -> Float -> Float -> (Float,Float)
raices a b c |sqrt(b^2-4*a*c)>= 0 = ((-b+sqrt(b^2-4*a*c))/(2*a),(-b-sqrt(b^2-4*a*c))/(2*a))

listar :: a -> a -> a -> [a]
listar a b c = [a,b,c]

pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente p q |fst p /= fst q = (snd p - snd q)/(fst p - fst q)
-- toma dos puntos y calcula la pendiente de la recta que pasa por ellos

iniciales :: String -> String -> String
iniciales "p" "q" = (head [p], head[q])
-- en la clase 7 vamos a ver una forma 
-- más complicada y piola de hacer la misma mierda

-- Dani --