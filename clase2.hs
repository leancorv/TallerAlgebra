{- |estanRelacionados: dados dos números reales, decide si están relacionados considerando
la relación de equivalencia en R cuyas clases de equivalencia son:
(−∞, 3], (3, 7] y (7, ∞) -}
estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | x > 3 && x <= 7 && y > 3 && y <= 7 = True
                      | x > 7 && y > 7 = True
                      | otherwise = False

{- |prodInt: calcula el producto interno entre dos vectores de R2 -}
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (v1, v2) (u1, u2) = v1 * u1 + v2 * u2

{- |todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer
vector es menor a la coordenada correspondiente del segundo vector -}
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (v1, v2) (u1, u2) | v1 < u1 && v2 < u2 = True
                            | otherwise = False

{- |distanciaPuntos: calcula la distancia entre dos puntos de R2 -}
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)

{- |sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos -}
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

{- |posicPrimerPar: dada una terna de enteros, devuelve la posición del primer número par si
es que hay alguno, y devuelve 4 si son todos impares -}
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | x `mod` 2 == 0 = 1
                         | y `mod` 2 == 0 = 2
                         | z `mod` 2 == 0 = 3
                         | otherwise = 4

{- |crearPar: crea un par a partir de sus dos componentes dadas por
separado (debe funcionar para elementos de cualquier tipo) -}
crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

{- |invertir: invierte los elementos del par pasado como parámetro
(debe funcionar para elementos de cualquier tipo) -}
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)