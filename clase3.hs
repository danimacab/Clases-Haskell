-- Clase 03 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- REDUCCIÓN Y RECURSIÓN --

resta :: Integer -> Integer -> Integer
resta x y = x - y

suma :: Integer -> Integer -> Integer
suma x y = x + y

digitos :: Integer -> Integer 
digitos x |x < 10 = 1
          |otherwise = 1 + digitos (x `div` 10)
-- div toma dos enteros y realiza una division entera entre ellos

suc :: Integer -> Integer
suc x = x + 1

inv :: Float -> Float
inv x | x /= 0 = 1/x

-- Definiciones recursivas
-- Aquí viene lo bueno, jóvenes! --

factorial :: Integer -> Integer
factorial n |n == 0 = 1
		      	|n > 0 = n * factorial (n - 1)

fib :: Integer -> Integer
fib n |n == 0 = 1
	    |n == 1 = 1
	    |otherwise = fib (n - 1) + fib (n - 2)

par :: Integer -> Bool
par n |n == 0 = True
      |n == 1 = False
      |otherwise = par (n - 2)

pa :: Integer -> Bool
pa n |n == 0 = True
     |otherwise = not (par (n - 1))

sumaImp :: Integer -> Integer
sumaImp n |n == 0 = 0
		  |otherwise = 2*n - 1

sumimp :: Integer -> Integer
sumimp n |n == 0 = 0
         |n == 1 = 1
         |otherwise = sumimp (n-1) + 2*n - 1

mult3 :: Integer -> Bool
mult3 n |n == 0 = False
        |n == 1 = False
        |n == 2 = False
        |n == 3 = True
        |otherwise = mult3 (n-3)

doblefact :: Integer -> Integer
doblefact n |n == 0 = 1
            |n > 1 = n * doblefact (n-2)

igualdad :: Integer -> Integer -> Integer
igualdad x y | y == 0 || x == y = 1
             |otherwise = (igualdad (x-1) y) + (igualdad (x-1) (y-1))

-- del ejercicio 3 no entendí un chori
-- el ejercicio 4 se resuelve al principio de la clase 4

-- Dani --