-- Clase 06 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- PATTERN MATCHING + TIPOS ENUMERADOS --

-- primera parte PATTERN MATCHING

not :: Bool -> Bool
not x | x == True = False
      | x == False = True
--si uso pattern matching:
no :: Bool -> Bool
no True  = False
no False = True

yLogico :: Bool -> Bool -> Bool
yLogico True True = True
yLogico _ _       = False

oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _         = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _        = True

sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana x = x + sumaGaussiana (x - 1)

algunoEsCero :: (Integer, Integer, Integer) -> Bool
algunoEsCero (0,y,z) = True
algunoEsCero (x,0,z) = True
algunoEsCero (x,y,0) = True
algunoEsCero (_,_,_) = False -- tambien vale poner un solo _ en vez de (_,_,_)
-- también vale 
-- algunoEsCero (x,y,z) = x * y * z == 0 

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

--segunda parte de la clase TIPOS ENUMERADOS
-- defino un tipo de datos: 

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving ( Eq , Ord , Show )

esFinde :: Dia -> Bool
esFinde Sabado  = True
esFinde Domingo = True
esFinde _       = False

esDiaHabil :: Dia -> Bool
esDiaHabil Sabado  = False
esDiaHabil Domingo = False
esDiaHabil _       = True

diaSiguiente :: Dia -> Dia
diaSiguiente Lunes = Martes
diaSiguiente Martes = Miercoles
diaSiguiente Miercoles = Jueves
diaSiguiente Jueves = Viernes 
diaSiguiente Viernes = Sabado
diaSiguiente Sabado = Domingo
diaSiguiente Domingo = Lunes
--compila pero tira un error:
 --No instance for (Show Dia) arising from a use of `print'
 --Possible fix: add an instance declaration for (Show Dia)
 --In a stmt of an interactive GHCi command: print it
--Esto significa que Haskell no tiene idea de como mostrar estos datos en pantalla
-- se resolvió agregando (a la definicion de Dia) deriving ( Eq , Ord , Show )

-- Ejercicio: Logo
-- La tortuga que no es tortuga. DESEPSIAAAN --

type Posicion = ( Integer , Integer )
data Direccion = Norte | Sur | Este | Oeste deriving ( Show )
type Tortuga = ( Posicion , Direccion )

arrancar :: Tortuga
arrancar = ((0,0) , Sur)

girarDerecha :: Tortuga -> Tortuga
girarDerecha (x , Norte) = (x , Este)
girarDerecha (x , Este) = (x , Sur)
girarDerecha (x , Sur) = (x , Oeste)
girarDerecha (x , Oeste) = (x , Norte)

avanzar :: Tortuga -> Integer -> Tortuga
avanzar ((x,y) , Norte) z = ((x,y+z) , Norte)
avanzar ((x,y) , Este) z = ((x+z,y) , Este)
avanzar ((x,y) , Sur) z = ((x,y-z) , Sur)
avanzar ((x,y) , Oeste) z = ((x-z,y) , Oeste)

-- Dani --