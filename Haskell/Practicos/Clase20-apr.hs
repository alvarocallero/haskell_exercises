--Ejercicio 4
--(a)
--Sin foldr 
elem_re x [] = False
elem_re x (y:xs) = (x == y) || elem_re x xs

--La que hizo un companero con foldr
ele1::Eq a=>a->[a]->Bool
ele1 x (y:ys) = foldr (\z w -> (z == x)||w) False (y:ys)

elem_fr x = foldr (\y r -> x == y || r) False

--ahora definirlo como foldl, usando un acumulador, el caso base es el inicializador del acumulador
--elem_fl x - foldl (\acc y -> x == y || acc) False


--ele es acumilativa local, es como hacer un for sobre la lista
elem_re' x xs = ele False xs
    where ele acc []     = acc
          ele acc (y:ys) = ele (acc || x == y) ys

-- es como un loop, tiene recursion de cola
-- fold h a [] = a
-- fold h a (y:ys) = fold h (h a y) ys

-- Recursion standard
--foldr h y [] = v
--foldr h v (y:ys) = h y (foldr h v ys)
{-
Por que esta mal definir la funcion reverse asi?:
reverse'::[a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs : x
-}
--reverse' (x:xs) = reverse' xs ++ [x]


--(c) Eliminar duplicados
--Primero la definicion recursiva explicita
elimDups [] = []
elimDups [x] = [x]
elimDups (x:y:xs) | x == y = elimDups (y:xs)
                  | otherwise = x : elimDups (y:xs)


--otra forma de escribirlo con recursion explicita:
elimDups_re::Eq a => [a] -> [a]
elimDups_re [] = []
elimDups_re (x:xs) = case elimDups_re xs of
                    [] -> [x]
                    zx@(y:ys) | x == y -> zs
                              | otherwise -> x : zs

--ahora lo escribimos como foldr, la idea es tomar como caso base la lista vacia, y el caso base de la lista con un solo elemento, meterlo
--dentro de la recursion
{-
elimDups_fr = foldr h []
        where h x r = case r of
            []                 -> [x]
            zx@(y:ys) | x == y -> zs
                      | otherwise -> x : zs

--otra con fold de un companero
elimDups_fr' :: Eq a => [a] -> [a]
elimDups_fr' = foldr elimPeg []
 where elimPeg x [] = [x]
       elimPeg x r = if x==head r then r else x : r


-}
--Por que esta mal definir la funcion reverse asi?:
reverse'::[a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs : x