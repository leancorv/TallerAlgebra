module EjerciciosClase3 where

    {- |fib : Z≥0 → Z que devuelve el i-ésimo número de Fibonacci -}
    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n | n > 0 = (fib (n - 1)) + (fib (n - 2))
          | otherwise = undefined

    {- |Escribir una función para determinar si un número natural es múltiplo de 3. No está permitido utilizar mod ni div -}
    multiplo3 :: Int -> Bool
    multiplo3 n | n == 3 = True
                | n > 3 = multiplo3 (n - 3)
                | otherwise = False

    {- |sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n números impares -}
    sumaImpares :: Int -> Int
    sumaImpares 0 = 0
    sumaImpares n | n == 1 = 1
                  | n > 1 = sumaImpares (n - 1) + n + (n - 1)
                  | otherwise = undefined

    {- |medioFact: dado n ∈ N calcula n!! = n (n − 2)(n − 4) -}
    medioFact :: Int -> Int
    medioFact n | n == 1 = 1
                | n == 2 = 2
                | n > 2 = n * medioFact (n - 2)
                | otherwise = undefined

    {- |Escribir una función que determine la suma de dígitos de un número positivo. Para esta función pueden utilizar div y mod -}
    sumaDig :: Int -> Int
    sumaDig n | n < 0 = undefined
              | n `div` 10 < 1 = n
              | otherwise = n `mod` 10 + (sumaDig (n `div` 10))

    {- |Implementar una función que determine si todos los dígitos de un número son iguales -}
    eqDig :: Int -> Bool
    eqDig n | n < 0 = undefined
            | n `div` 10 < 1 = True
            | n `mod` 10 == ((n `div` 10) `mod` 10) && eqDig (n `div` 10) = True
            | otherwise = False