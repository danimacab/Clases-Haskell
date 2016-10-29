-- Clase 04 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- empezamos resolviendo el último ejercicio de la clase 3

sumAuxiliar :: Integer -> Integer -> Integer
sumAuxiliar umbral num | (num ^ 2) >= umbral = 0
                       |otherwise = num + sumAuxiliar umbral (num + 2)

sumImpar :: Integer -> Integer
sumImpar umbral = sumAuxiliar umbral 1

-- RECURSIÓN SOBRE LISTAS --

productoria :: [Integer] -> Integer
productoria lista | length lista == 0 = 1
                  | otherwise = (head lista) * productoria (tail lista)

reverso :: [Integer] -> [Integer]
reverso lista | length lista == 0 = []
              | otherwise = reverso (tail lista) ++ [head lista]

pertenece :: Integer -> [Integer] -> Bool
pertenece x lista | length lista == 0 = False
                  | head lista == x = True
                  | otherwise = pertenece x (tail lista)

hayRepetidos :: [Integer] -> Bool
hayRepetidos lista | length lista <= 1 = False
                   | pertenece (head lista) (tail lista) == True = True
                   | otherwise = hayRepetidos (tail lista)

quitar :: Integer -> [Integer] -> [Integer]
quitar x lista | length lista == 0 = []
               | (head lista) == x = tail lista
               | (head lista) /= x = [head lista] ++ quitar x (tail lista)

elimRep :: [Integer] -> [Integer]
elimRep lista | length lista == 0 = []
              | pertenece (head lista) (tail lista) = elimRep (tail lista)
              | otherwise = [head lista] ++ elimRep (tail lista)

maximo :: [Integer] -> Integer
maximo lista | length lista == 1 = (head lista)
             | (head lista) > maximo (tail lista) = (head lista)
             | (head lista) < maximo (tail lista) = maximo (tail lista)
-- puedo poner (por ejemplo) where q= maximo (blablabla) y usar q en vez del choclo

ordenar :: [Integer] -> [Integer]
ordenar lista | length lista == 0 = [] 
              | otherwise = (maximo lista) : ordenar (quitar (maximo lista) lista)

suma :: [Integer] -> [Integer] -> [Integer]
suma p q | (length p) /= (length q) = []
         | otherwise = [(head p) + (head q)] ++ suma (tail p) (tail q)

prodInterno :: [Float] -> [Float] -> Float
prodInterno p q | (length p) /= (length q) = []
                | otherwise = (head p) * (head q) + prodInterno (tail p) (tail q)

-- faltan dos ejercicios adicionales. Paja.

-- Dani --