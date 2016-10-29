-- Clase 01 - Taller de Álgebra - 2do Cuatrimestre 2016
-- Todo lo que se ve acá se pudo compilar y funciona super tranquipiola
-- Eso no significa que esté del todo bien,
-- ni que las aclaraciones sean correctas.

-- INTRODUCCIÓN A LA PROGRAMACIÓN FUNCIONAL --

suma x y = x - y

doble x = 2*x

normaVectorial v1 v2 = sqrt((v1)^2+(v2)^2)

funcionConstante8 x = 8

respuestaATodo = 42

signo n |n > 0 = 1
        |n == 0 = 0
        |n < 0 = (-1)

valorAbsoluto x |x >= 0 = x
                |x < 0 = (-x)

maximo x y |x >= y = x
           |x < y = y

maximo3 x y z |(x>=y)&&(y>=z) = x
              |(x>=z)&&(z>=y) = x
              |(y>=z)&&(z>=x) = y
              |(y>=x)&&(x>=z) = y
              |(z>=x)&&(x>=y) = z
              |(z>=y)&&(y>=x) = z
-- "&&" representa al Y lógico, mientras que
-- "||" representa al O lógico

delta n |n == 0 = 1
        |n /= 0 = 0
-- "==" vendría a usarse para decir "si n es igual a 0..."
-- "/=" es lo mismo pero con distinto.

delta2 n |n == 0 = 1
         |otherwise = 0
-- "otherwise" representa a "cualquier otro caso"
-- no planteado en los casos anteriores (o sea, "si no").
-- Tener cuidado con el orden de las guardas, 
-- porque si las condiciones se solapan, puede cambiar el comportamiento de la función.

f x y z |y < 10 = x
        |y >= 10 = (x + z) 

raiz a b c = (((-b)+sqrt(b^2-4*a*c))/(2*a),
             ((-b)-sqrt(b^2-4*a*c))/(2*a))

-- Dani --