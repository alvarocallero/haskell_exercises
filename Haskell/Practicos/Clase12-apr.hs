--Ejercicio 6
{-
dup . dup te da un par de pares
(dup dup) a la funcion dup le paso como argumento dup

El tipo de dup es a -> (a,a)
El tipo de dup . dup es a -> ((a,a),(a,a))

dup dup :: (a -> (a,a), a-> (a,a))

-}

--Ejercicio 10
{-
cantPares = length . filter _, el _ te sugiere que tipo debe ser lo que recibe esa funcion
-}

cantPares::[Int] -> Int
cantPares = length . filter ((==0) . (`mod` 2))
cantPares' = length . filter ((==0) . (`mod` 2))

cantPares''' xs = length . filter (==0) . map (flip mod 2) $ xs

cantPares'' = map (flip mod 2)

--Ejercicio 14
m = [[1,2,3],[4,5,6]]
-- m   |1 2 3|
--     |4 5 6|
type Matriz a = [[a]]
fila :: Int -> Matriz a -> [a]
fila n m = m !! (n-1)
fila' n = head . drop (n-1) 

columna::Int -> Matriz a -> [a]
columna n m = [head(drop (n-1) z) | z <-m]

columna' n m = map (head . (drop(n-1)))

foo (x:xs) = x ++ []

--Ejercicio 11
h::tx -> ty -> a
g::tx -> (ty -> b)
f::b -> a

h x y = f (g x y)



h x y = f ((g x) y)

==> reescribo segun (f . g) x == f (g x)

h x y = (f . g x) y

===> eta reduccion

h x = f . g x





--ni la A ni la C tienen sentido
--(f . g) --- es ill-typed


--Ejercicio 13
k = [case t of {(Iso _ _ )-> True; _ -> False } | t<- [Iso] ::[Triangulo]]