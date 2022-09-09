
factorial :: Int -> Int
factorial n | n == 0 = 1 -- caso base
            | n  > 0 = n * factorial (n - 1)

-- para evitar la recursión infinita
factorial2 :: Int -> Int
factorial2 n | n == 0 = 1 -- caso base
             | n  > 0 = n * factorial (n - 1)
             | otherwise = undefined 

-- pattern matching
factorial3 :: Int -> Int
factorial3 0 = 1 -- caso base
factorial3 n = n * factorial (n - 1)

{- `n` no es un dato en memoria, varía su valor en el transcurso de la ejecución de la función. No es una variable. Sólo es un nombre para el parámetro que toma la función.

DEF: Una función definida en términos de ella misma. Debe haber un caso base para evitar una recursión al infinito. -}

multiplo3 :: Int -> Bool
multiplo3 n | n == 3 = True
            | n > 3 = multiplo3 (n - 3)
            | otherwise = False
