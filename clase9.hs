-- Clase 09 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- REPASO. PATTERN MATCHING SOBRE LISTAS --

tuplas :: [a] -> [b] -> [(a , b)]
tuplas [] [] = []
tuplas _ [] = []
tuplas [] _ = []
tuplas (x:xs) (y:ys) = [(x , y)] ++ tuplas (xs) (ys)
-- Al principio hice esto
-- tuplas x y = [(head x , head y)] ++ tuplas (tail x) (tail y)
-- pero la idea es plantear las listas con la estructura "(x:xs)"
-- entonces x es el head y xs el tail de la lista

intercalar :: [a] -> [a] -> [a]
intercalar [] [] = []
intercalar [] ys = ys
intercalar xs [] = xs
intercalar (x:xs) (y:ys) = [x , y] ++ intercalar (xs) (ys)
-- Tambien había hecho esto, pero idem
-- intercalar x y = [head x , head y] ++ intercalar (tail x) (tail y)

tomarN :: Integer -> [a] -> [a]
tomarN n (x:[]) = [x]
tomarN 1 (x:xs) = [x]
tomarN n (x:xs) = x : tomarN (n-1) xs
-- toma los n primeros elementos de una lista

sumaDeListas :: [Integer] -> [Integer] -> [Integer]
sumaDeListas xs [] = xs
sumaDeListas [] ys = ys
sumaDeListas (x:[]) (y:[]) = [x+y]
sumaDeListas (x:xs) (y:ys) = (x+y) : sumaDeListas xs ys
-- hace una lista de la suma de elementos de dos listas

unoMasDosIgualTres :: [Integer] -> Bool
unoMasDosIgualTres (x:y:z:xs) = x+y == z
unoMasDosIgualTres _ = False 
-- suma los dos primeros elementos, y  se fija si danel tercero

listasIguales :: Eq a => [a] -> [a] -> Bool
listasIguales xs [] = False
listasIguales [] ys = False
listasIguales (x:[]) (y:[]) = x == y 
listasIguales (x:xs) (y:ys) = x == y && listasIguales xs ys
-- se fija si dos listas son exactamente iguales

cantidadDeApariciones :: Eq a => a -> [a] -> Integer
cantidadDeApariciones n [] = 0
cantidadDeApariciones n (x:xs) | n == x = 1 + cantidadDeApariciones n xs
                               | n /= x = cantidadDeApariciones n xs
-- se fija cuantas veces aparece un elemento n en una lista

-- Dani --