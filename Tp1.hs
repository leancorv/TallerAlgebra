-- Apellido Nombre #1
-- Apellido Nombre #2
-- Apellido Nombre #3


-- EJERCICIO 1. sonCoprimos
-- sonCoprimos :: Integer -> Integer -> Bool
{- Dados dos números naturales, a y b, es posible calcular su máximo común divisor mediante el Algoritmo de Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
mcd(a,b) = a,                   si b = 0
         = mcd (b, a módulo b), si b > 0

Definimos la función 
mcd :: Integer -> Integer -> Integer
tal que (mcd a b) es el máximo común divisor de a y b calculado mediante el algoritmo de Euclides. Por ejemplo,
mcd 30 45  ==  15 
(Me parece que no hace falta describir las funciones pero igual lo puse, después se lo podemos sacar si quieres)
-}
 
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1

{-Usando gcd de haskell-}
sonCoprimosHaskell :: Integer -> Integer -> Bool
sonCoprimosHaskell a b = gcd a b == 1

-- EJE3RCICO 2: es2Pseudoprimo
--es2Pseudoprimo :: Integer -> Bool
{-Función auxiliar (Más en general)-}
esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo a n = mcd a n == 1 && (a^(n-1)-1) `mod` n == 0

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esAPseudoprimo 2 n

-- EJERCICIO 3: cantidad3Pseudoprimos
--cantidad3Pseudoprimos :: Integer -> Integer
--meme

-- EJERCICIO 4: kesimo2y3Pseudoprimo
--kesimo2y3Pseudoprimo :: Integer -> Integer


-- EJERCICIO 5: esCarmichael
--esCarmichael :: Integer -> Bool
{-Opción 1- Sacada de telegram hecha en 2020-}
checkAPseudo :: Integer -> Integer -> Bool
checkAPseudo a n | a >= n-1 = True
                 | not (esAPseudoprimo a n) && sonCoprimos a n = False
                 | otherwise = checkAPseudo (a+1) n

esCarmichael :: Integer -> Bool
esCarmichael n = checkAPseudo 2 n

{-
Opción 2 (Es practicamente igual pero creo que solo cambia que esCarmichael la manda con la auxiliar pero empezando desde 1 (esCarmichaelAux 1 n) y en el otro lo hace con 2 n) igual funciona xD
-}
esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux a n | a == n-1 && esAPseudoprimo a n = True
                     | sonCoprimos a n && not(esAPseudoprimo a n) = False
                     | otherwise = esCarmichaelAux (a+1) n

esCarmichael' :: Integer -> Bool
esCarmichael' n = esCarmichaelAux 1 n 