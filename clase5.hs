-- Clase 05 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- Empieza con todo lo de la clase 04 que iba a necesitar para esta clase. 

pertenece :: Integer -> [Integer] -> Bool
pertenece x lista |length lista == 0 = False
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

ordenar :: [Integer] -> [Integer]
ordenar lista | length lista == 0 = [] 
              | otherwise = (maximo lista) : ordenar (quitar (maximo lista) lista)

-- clase 05 (ahora si)
-- RENOMBRES DE TIPOS + MÁS RECURSIÓN SOBRE LISTAS --

type Conjunto = [Integer]

eliminarElemento :: Integer -> Conjunto -> Conjunto
eliminarElemento n conj = quitar n conj

agregarElemento :: Integer -> Conjunto -> Conjunto
agregarElemento n conj = elimRep ([n] ++ conj)

union :: Conjunto -> Conjunto -> Conjunto
union a b = elimRep (a ++ b) 

interseccion :: Conjunto -> Conjunto -> Conjunto
interseccion a b | length a == 0 = []
                 | pertenece (head a) b = [head a] ++ interseccion (tail a) b
                 | otherwise = interseccion (tail a) b
--tambien sirve sin el caso base

inclus :: Conjunto -> Conjunto -> Bool
inclus a b | length a == 0 = True 
           | interseccion a b == a = True
           | otherwise = False
-- esto no sirve porque no vale cuando los elementos de la lista están desordenados. Es mejor comparar la cantidad de elementos de la interseccion con a
inclusion :: Conjunto -> Conjunto -> Bool
inclusion a b | length a == 0 = True 
              | length (interseccion a b) == length a = True
              | otherwise = False

igualdadConjunto :: Conjunto -> Conjunto -> Bool
igualdadConjunto a b | inclusion a b && inclusion b a = True
                     | otherwise = False
-- está bien pero poner otherwise es redundante. Vale tambien:
igualdadConjun2 :: Conjunto -> Conjunto -> Bool
igualdadConjun2 a b = inclusion a b && inclusion b a 

difConjuntos :: Conjunto -> Conjunto -> Conjunto
difConjuntos a b | length b == 0 = a
                 | pertenece (head b) a = difConjuntos (quitar (head b) a) (tail b)
                 | otherwise = difConjuntos a (tail b)

difSimetrica :: Conjunto -> Conjunto -> Conjunto
difSimetrica a b = difConjuntos (union a b) (interseccion a b)

intersecMultiple :: [Conjunto] -> Conjunto
intersecMultiple lista | length lista <= 1 = head lista
                       | otherwise = interseccion (head lista) (intersecMultiple (tail lista))

unionMultiple :: [Conjunto] -> Conjunto
unionMultiple lista | length lista <= 1 = head lista
                    | otherwise = union (head lista) (unionMultiple (tail lista))

mayoresQue :: Integer -> Conjunto -> Conjunto
mayoresQue x lista | length lista == 0 = []
                   | (head lista) <= x = mayoresQue x (quitar (head lista) lista) 
                   | (head lista) > x = [head lista] ++ (mayoresQue x (tail lista)) 

conjPares :: Conjunto -> Conjunto
conjPares lista | length lista == 0 = []
                | mod (head lista) 2 == 0 = [head lista] ++ conjPares (tail lista)
                | mod (head lista) 2 /= 0 = conjPares (tail lista) 
conjImp :: Conjunto -> Conjunto
conjImp lista | length lista == 0 = []
              | mod (head lista) 2 /= 0 = [head lista] ++ conjImp (tail lista)
              | mod (head lista) 2 == 0 = conjImp (tail lista) 
separaParImpar :: Conjunto -> (Conjunto,Conjunto)
separaParImpar lista = (conjPares lista , conjImp lista)

valAbs :: Conjunto -> Conjunto
valAbs lista | length lista == 0 = []
             | otherwise = elimRep (ordenar ([head lista] ++ [-(head lista)] ++ (valAbs (tail lista))))

-- Dani --