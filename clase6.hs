{- Escribir una expresi ́on que denote la lista estrictamente decreciente de enteros que comienza
con el n ́umero 1 y termina con el n ́umero -100. -}
l = [1,0..(-100)]

{-Escribir una expresi ́on que denote la lista estrictamente creciente de enteros entre −20 y 20
que son congruentes a 1 m ́odulo 4-}
l2 = [(-19),(-15)..20]

{-sumatoria :: [Int] -> Int
que indica la suma de los elementos de una lista.-}

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria xs = head xs + sumatoria (tail xs) 

{-longitud :: [Int] -> Int
que indica cu ́antos elementos tiene una lista.-}
longitud :: [Int] -> Int
longitud [] = 0
longitud xs = 1 + longitud (tail xs) 

{-3 pertenece :: Int -> [Int] -> Bool
que indica si un elemento aparece en la lista-}

pertenece :: Int -> [Int] -> Bool
pertenece a xs | xs == [] = False
               | otherwise = a == head xs || pertenece a (tail xs)
               
pertenecePM :: Int -> [Int] -> Bool   
pertenecePM e [] = False
pertenecePM e (x:xs) = e == x || pertenecePM e xs

productoria :: [Int] -> Int
productoria ab | ab == [] = 1
               | otherwise = (head ab)*(productoria (tail ab))

{- sumarN :: Int -> [Int] -> [Int] que dado un n ́umero N y una lista xs, suma N a cada
elemento de xs-}

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (x+n) : sumarN n xs

{-sumarElPrimero :: [Int] -> [Int] que dada una lista no vac ́ıa xs, suma el primer
elemento a cada elemento de xs. Ejemplo: sumarElPrimero [1,2,3] ⇝ [2,3,4]-}

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (x:xs)= sumarN x (x:xs)

{-sumarElUltimo :: [Int] -> [Int] que dada una lista no vac ́ıa xs, suma el  ́ultimo
elemento a cada elemento de xs. Ejemplo: sumarElUltimo [1,2,3] ⇝ [4,5,6]-}

buscarUltimo :: [Int] -> Int
buscarUltimo (x:xs) | longitud (x:xs) == 1 = x
                    | otherwise = buscarUltimo xs
                    
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo (x:xs) = sumarN (buscarUltimo (x:xs)) (x:xs)

{-pares :: [Int] -> [Int] que devuelve una lista con los elementos pares de una lista
dada. Ejemplo: pares [1,2,3,5,8] ⇝ [2,8]-}

{-multiplosDeN :: Int -> [Int] -> [Int] que dado un n ́umero N y una lista xs, devuelve
una lista con los elementos m ́ultiplos N de xs-}
esMultiploDeN :: Int -> Int -> Bool|
esMultiploDeN n x = mod x n == 0

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | esMultiploDeN n x == True = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

{-reverso :: [Int] -> [Int] que dada una lista invierte su orden-}


