-- Clase 07 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- TIPOS PARAMÉTRICOS Y RECURSIVOS --

data Vector = Vector2D Float Float | Vector3D Float Float Float deriving (Show)

data Punto = Punto2D Float Float deriving (Show)

data Figura = Rectangulo Punto Punto | Circulo Punto Float deriving (Show)

-- no olvidar el deriving (Show) al final de las definiciones

normaVectorial :: Vector -> Float
normaVectorial (Vector2D x y)   = sqrt ((x^2) + (y^2))
normaVectorial (Vector3D x y z) = sqrt ((x^2) + (y^2) + (z^2)) 
-- cuando quiera escribir en la terminal voy a tener que hacer 
-- >normaVectorial $ Vector2D x y o bien
-- >normaVectorial $ Vector3D x y z

circuloUnit :: Figura
circuloUnit = Circulo (Punto2D 0 0) 1
-- me pone un círculo de radio 1 en el origen.
-- o sea, sólo me está imprimiendo lo que escribí acá.

cuadrado :: Float -> Figura
cuadrado d = Rectangulo (Punto2D 0 0) (Punto2D (d/sqrt(2)) (d/sqrt(2)))
-- da un cuadrado (cuatro lados iguales -dah-) de diagonal d
-- ubicando una de sus esquinas en el origen

perimetro :: Figura -> Float
perimetro (Rectangulo (Punto2D x y) (Punto2D z w)) = 2 * abs(z-x) + 2 * abs(w-y)
perimetro (Circulo (Punto2D a b) r) = 2 * pi * r
-- en este caso se trata de escribir en la terminal
-- perimetro $ Rectangulo (Punto2D x y) (Punto2D z w)
-- perimetro $ Circulo (Punto2D a b) r
-- me acaban de decir que $ es un operador que te cambia la precedencia
-- o sea, dice algo así como: primero evalua todo lo que está a la derecha mío
-- y luego seguí. Por ejemplo, podés ahorrar paréntesis: 
-- y = 1 / (x + z) = 1 $ x + z
-- ni idea si me chamuyaron pero me ayudó a entender qué onda con ese símbolo

area :: Figura -> Float 
area (Rectangulo (Punto2D x y) (Punto2D z w)) = abs(z-x) * abs(w-y)
area (Circulo _ r) = pi * r^2
-- como el punto no me importa, en vez de especificar
-- (Punto2D a b) uso un guion bajo y ya fue

data ParOrdenado a b = Par a b deriving (Show)

primero :: ParOrdenado a b -> a 
primero (Par a _) = a

segundo :: ParOrdenado a b -> b
segundo (Par _ b) = b

data Lista a = ListaVacia | Agregar a (Lista a) deriving (Show)
-- Me tomó bocha de tiempo entender que de acá no salen
-- las listas de haskell como las conocemos. Digamos que éstas
-- vendrían a ser las listas Daniela.
-- Que nada que ver. Se ven espantosas. Como Daniela.

esVacia :: Lista a -> Bool 
esVacia ListaVacia = True
esVacia _ = False
-- acá la que va es poner
-- >esVacia ListaVacia y me devuelve True
-- >esVacia (Agregar x (ListaVacia)) y me devuelve False

cabeza :: Lista a -> a 
cabeza (Agregar x _) = x
-- vendría a ser el equivalente de Head para listas Daniela

cola :: Lista a -> Lista a
cola (Agregar _ y) = y
-- y este el equivalente de Tail

concatenar :: Lista a -> Lista a -> Lista a 
concatenar ListaVacia x = x
concatenar (Agregar a b) x = Agregar a (concatenar b x)
-- esto sería hacer (++)

longitud :: Lista a -> Integer
longitud ListaVacia = 0
longitud (Agregar a b) = 1 + longitud b
-- y esta es la funcion length

suma :: Lista Float -> Float
suma ListaVacia = 0
suma (Agregar x y) = x + suma y
-- acá sumo lo que vendrían a ser los elementos de la lista Daniela

posicion :: Lista a -> Integer -> a
posicion (Agregar a _) 1 = a
posicion (Agregar _ b) n = posicion b (n-1)
-- acá no tengo la mas puta idea de cuál era la intención del ejercicio.
-- me copié del codeo de otro chico pero tampoco entiendo qué es lo que pasa
-- aunque, claro. Compila y devuelve algo

-- pattern matching en listas --

producto :: [Integer] -> Integer
producto xs | length xs == 0 = 1
            | otherwise      = head xs * producto (tail xs)
-- es lo mismo que 
producto' :: [Integer] -> Integer
producto' []       = 1
producto' (x : xs) = x * producto' xs
-- esta funcion simplemente hace producto entre los elementos de una lista

longitud' :: [a] -> Integer
longitud' []               = 0
longitud' (x : [])         = 1
longitud' (x : y : [])     = 2
longitud' (x : y : z : []) = 3
longitud' (_ : _ : _ : xs) = 3 + longitud' (head xs : tail xs)
-- este es otro lenght... más rebuscado

iniciales :: [Char] -> [Char] -> [Char]
iniciales nombre apellido = [ n , a ]
          where ( n : _ ) = nombre
                ( a : _ ) = apellido

tuplas :: [a] -> [b] -> [(a , b)]
tuplas [] [] = []
tuplas _ [] = []
tuplas [] _ = []
tuplas x y = [(head x , head y)] ++ tuplas (tail x) (tail y)
-- arma tuplas donde las primeras componentes son de la primera lista 
-- y las segundas de la segunda

intercalar :: [a] -> [a] -> [a]
intercalar [] [] = []
intercalar [] y = y
intercalar x [] = x
intercalar x y = [head x , head y] ++ intercalar (tail x) (tail y)
-- intercala los elementos de ambas listas

-- Dani --