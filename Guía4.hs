--Ejercicio 1:

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

--Ejercicio 2:

parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0
              | x >= 1 = 1 + parteEntera ( x - 1 )

--Ejercicio 3:

restaSucesiva :: Integer -> Integer -> Integer -- Si a esta función le meto 15 y 2 va a hacer x = (15 - 2 - 2 ...- 2) hasta que x sea menor o igual a y
restaSucesiva x y | x == y = 0
                  | x < y = -1
                  | otherwise = restaSucesiva (x - y) y

esDivisible :: Integer -> Integer -> Bool
esDivisible x y | restaSucesiva x y == 0 = True
                | otherwise = False

--Ejercicio 4:

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | n > 1 = (2 * (n - 1) + 1) + sumaImpares (n - 1)

--Ejercicio 5:

medioFact :: Integer -> Integer
medioFact n | n == 0 || n == 1 = 1
            | n == 2 = 2
            | n > 0 = n * medioFact (n - 2)

--Ejercicio 6:

--todosDigitosIguales :: Integer -> Integer
--todosDigitosIguales n | (div n 10) < 10 = True
 --                     | (div n 10) >= 10 = div n 10
--
--Ejercicio 14:

auxiliarN :: Integer -> Integer -> Integer -> Integer --Suma las columnas
auxiliarN q n m | n == 1 = q ^ (1 + m)
                | otherwise = q ^ (n + m) + (auxiliarN q (n - 1)  m)

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n 1 = auxiliarN q n 1
sumaPotencias q n m = sumaPotencias q n (m - 1) + auxiliarN q n m

--Ejercicio 16:

menorDivisorDesde :: Integer -> Integer -> Integer 
menorDivisorDesde k n | mod n k == 0 = k
                      | otherwise = menorDivisorDesde (k + 1) n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde 2 n

--Ejercicio 16:

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n --Es primo si el menor divisor que tiene n es él mismo.

esSumaDePrimerosMPrimos :: Int -> Int -> Bool
esSumaDePrimerosMPrimos m n | m == n = True
                            | m > n = False
                            | otherwise = esSumaDePrimerosMPrimos (m + 1) n

sumaPrimosDesde :: Integer -> Integer -> Integer
sumaPrimosDesde m n | 
                    | esPrimo n = n + sumaPrimosDesde (n - 1)


