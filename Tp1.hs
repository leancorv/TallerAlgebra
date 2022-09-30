-- Apellido Nombre #1
-- Apellido Nombre #2
-- Apellido Nombre #3


-- EJERCICIO 1. sonCoprimos
{-Usando gcd de haskell-}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = gcd a b == 1

{-
Dados dos números naturales, a y b, es posible
calcular su máximo común divisor mediante el Algoritmo de
Euclides. Este algoritmo se puede resumir en la siguiente fórmula:
mcd(a,b) = a,                   si b = 0
         = mcd (b, a módulo b), si b > 0

Definimos la función 
mcd :: Integer -> Integer -> Integer
tal que (mcd a b) es el máximo común divisor de a y b calculado
mediante el algoritmo de Euclides. Por ejemplo,
mcd 30 45  ==  15
-}
 
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

sonCoprimos' :: Integer -> Integer -> Bool
sonCoprimos' a b = mcd a b == 1

-- EJE3RCICO 2: es2Pseudoprimo
--es2Pseudoprimo :: Integer -> Bool
{-Función auxiliar (Más en general)-}
esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo a n = mcd a n == 1 && (a^(n-1)-1) `mod` n == 0

es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n = esAPseudoprimo 2 n

-- EJERCICIO 3: cantidad3Pseudoprimos
--cantidad3Pseudoprimos :: Integer -> Integer


-- EJERCICIO 4: kesimo2y3Pseudoprimo
--kesimo2y3Pseudoprimo :: Integer -> Integer


-- EJERCICIO 5: esCarmichael
--esCarmichael :: Integer -> Bool

checkAPseudo :: Integer -> Integer -> Bool
checkAPseudo a n | a >= n-1 = True
                 | not (esAPseudoprimo a n) && sonCoprimos a n = False
                 | otherwise = checkAPseudo (a+1) n
{-
esCarmichaelAux :: Integer -> Integer -> Bool
esCarmichaelAux n a | a == n-1 && esAPseudoPrimo n a = True
                     | sonCoprimos n a && not(esAPseudoPrimo n a) = False
                     | otherwise = esCarmichaelAux n (a+1)

esCarmichael :: Integer -> Bool
esCarmichael n = esCarmichaelAux n 1
-}
esCarmichael :: Integer -> Bool
esCarmichael n = checkAPseudo 2 n