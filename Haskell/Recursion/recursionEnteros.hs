--RECURSION SOBRE ENTEROS
-------------------------

--Factorial
fact 0 = 1
fact n = n * fact (n-1)

fact'::Integer -> Integer
fact' n | n == 0 = 1
        | n > 0  = n *fact'(n-1)

fact''::Integer -> Integer
fact'' n | n == 0 = 1
         | otherwise = n *fact'(n-1)

fact''' 0 = 1
fact''' n = n * fact'''(n-1)

--Potencia de 2
pot2:: Integer -> Integer
pot2 0 = 1
pot2 n = 2 * pot2(n-1)

--Sumatoria de los valores de una funcion
--Me dan como parametro una funcion f y voy a computar la sumatoria de los valores en el intervalo (n, n-1,... ,0) es decir,
--f(n) + f(n-1) + f(n-2) + ... + f(0)
sumF::(Integer -> Integer ) -> Integer  -> Integer 
sumF f 0 = f 0
sumF f n = f n + sumF f(n-1)

--Fibonacci
fibo::Integer -> Integer
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo(n-2)

--Definir de forma recursiva la funcion maximum
elMaximum::(Ord a) => [a] -> a
elMaximum [x] = x
elMaximum (x:xs) 
                | x > maxTail = x
                | otherwise   = maxTail
                where maxTail = elMaximum xs 

