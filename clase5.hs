sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n


sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n = n
                       | k < n && n `mod` k == 0 = k + sumaDivisoresDesde n (k+1)
                       | k > n && n `mod` k /= 0 = sumaDivisoresDesde n (k+1)

-- sumaDivisores' :: Int -> Int
-- sumaDivisores' n = sumaDivisoresDesde n 1
--3
menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | n `mod` k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1)
                      
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

--4
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

--5
minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

-- 6
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m | (fact i) >= m = fact i
                        | otherwise = menorFactDesdeDesde (i+1) m

menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m

--7
--mayorFactHasta :: Int -> Int




















