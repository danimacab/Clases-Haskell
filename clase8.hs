-- Clase 08 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- DIVISIBILIDAD Y CONGRUENCIA --

division :: Integer -> Integer -> (Integer,Integer)
division a d | a <  d = (0 , a)
             | a >= d = (1+ fst (division (a-d) d) , snd (division (a-d) d))
--acá hay un problema con el largo de la función. Otra opcion puede ser 
division' a d | a <  d = (0 , a)
              | a >= d = (1+ fst anterior , snd anterior)
              where anterior = division' (a-d) d


divParcial :: Integer -> Integer -> [Integer]
divParcial n m |m == 1 = [1]
               |mod n m /= 0 = divParcial n (m-1) 
               |mod n m == 0 = m : divParcial n (m-1)

divisores :: Integer -> [Integer]
divisores n = divParcial n n

esPrimo :: Integer -> Bool
esPrimo n | length (divisores n) == 2 = True
          | otherwise = False

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (snd (division a b))

euclides :: Integer -> Integer -> (Integer,Integer)
euclides a 0 = (1 , 0)
euclides a b = (t , s - t*q)
              where (s,t) = euclides b r
                    (q,r) = division' a b

data ClaseCongr = Vacio | CongruentesA Integer Integer deriving (Show)

multiplo :: Integer -> Integer -> Bool
multiplo a b | b == 0 = False
             | mod a b == 0 = True
             | otherwise = False

congruentes :: Integer -> Integer -> Integer -> Bool
congruentes a b m | mod (a-b) m == 0 = True
                  | otherwise = False

pertenece :: Integer -> ClaseCongr -> Bool
pertenece x Vacio = False 
pertenece a (CongruentesA b m) = congruentes a b m

incluido :: ClaseCongr -> ClaseCongr -> Bool
incluido Vacio (CongruentesA b m) = True
incluido _ Vacio = False
incluido (CongruentesA b1 m1) (CongruentesA b2 m2) = congruentes b1 b2 m2

-- faltan un monton de ejercicios pero ni siquiera los vimos

-- Dani --