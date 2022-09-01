suma a b = a + b
doble x = 2 * x
normaVec x1 x2 = sqrt (x1^2 + x2^2)
cte8 x = 8


-- pattern matching
-- Si evalúo f en 0 me va a devolver 1, porque va a escapar
-- el programa luego de la primera línea
-- Si n /= 0 va a evaluar la segunda línea.
f 0 = 1
f n = 0


signo n | n > 0 = 1
        | n == 0 = 0
        | otherwise = -1

maximo x y | x >= y = x
           | otherwise = y

-- Non-exhaustive patterns, la función no está definida para los reales < 3
f1 n | n >= 3 = 5

-- Podemos explicitar con undefined
f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

-- Cuando dos guardas se solapan su orden define el comportamiento de la función
-- Si n > 4, para f4 el la imagen de n es 5, para f5 es 7
f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5

cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
                         where d = b ^2 - 4* c

--Tipos de Datos
-- Especificar signatura de una función
esPar :: Int -> Bool
esPar n | mod n 2 == 0 = True
        | otherwise = False

esImpar :: Int -> Bool
esImpar n = not (esPar n)

{- |absoluto:
devuelve el valor absoluto de un número entero -}
absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | x < 0 = x * (-1)
           | otherwise = undefined -- this line makes no absolute sense but it makes the extension shut the fuck up so it'll stay

{- |maximoabsoluto: 
devuelve el máximo entre el valor absoluto de dos números enteros -}
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | a > b = a
                   | b > a = b
                   | otherwise = undefined
               where a = absoluto x
                     b = absoluto y

{- |maximo3: 
devuelve el máximo entre tres números enteros -}
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z
              | otherwise = undefined 

{- |algunoEs0: 
dados dos números racionales, decide si alguno de los dos es igual a 0
(hacerlo dos veces, una sin usar y otra usando pattern matching) -}
--Con Pattern matching
algunoEs0 :: Float -> Float -> Bool
algunoEs0 0 _ = True
algunoEs0 _ 0 = True
algunoEs0 _ _ = False

--Sin Pattern matching
algunoEs02 :: Float -> Float -> Bool
algunoEs02 x y | x == 0 = True
               | y == 0 = True
               | otherwise = False

{- |ambosSon0:
dados dos números racionales, decide si ambos son iguales a 0 
(hacerlo dos veces, una sin usar y otra usando pattern matching) -}
ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

--Sin Pattern matching
ambosSon02 :: Float -> Float -> Bool
ambosSon02 x y | x == 0 && y == 0 = True
               | otherwise = False

{- |esMultiploDe:
dados dos números naturales, decidir si el primero es múltiplo del segundo -}
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | mod y x == 0 = True
                 | otherwise = False

{- |digitoUnidades:
dado un número natural, extrae su dígito de las unidades -}
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

{- |digitoDecenas: 
dado un número natural, extrae su dígito de las decenas -}
digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10