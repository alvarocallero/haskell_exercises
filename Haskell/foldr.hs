--Entendiendo el foldr
{-
La manera mas facil de entender foldr es re escribiendo la lista de forma no azucarada:

[1,2,3,4,5] => 1:(2:(3:(4:(5:[]))))

ahora lo que el foldr f x hace, es reemplazar cada : por f de forma infija, y [] con la x y evalua el resultado
Por ejemplo:

sum [1,2,3] = foldr (+) 0 [1,2,3]

[1,2,3] === 1:(2:(3:[]))

Entonces lo que obtenemos es lo siguiente:
sum [1,2,3] === 1+(2+(3+0)) = 6
-}

--Definicion de foldr
foldr'::(a -> b -> b) -> b -> [a] -> b
foldr' f e []     = e
foldr' f e (x:xs) = f x (foldr' f e xs)

--Si una funcion recursiva estructural tiene esta forma, se puede escribir como foldr
{- 
h::[a] -> b
h []     = e
h (x:xs) = f x (h xs)
-}

--Escribir length como foldr
length' [] = 0
lenght' (x:xs) = 1 + lenght' xs

--lenght'' = foldr flen 0
--    where flen _ r = 1 + r

--Escribir all como foldr
all' p [] = True
all' p (x:xs) = p x && all' p xs

allFoldr p = foldr f True
    where f x r = p x && r

--Definir conmapcat como foldr
map' :: (a -> b) -> [a] -> [b]
map' f [ ] = [ ]
map' f (x : xs) = f x : map' f xs

mapF h = foldr f []
    where f x r = h x : r

filter' p [ ] = [ ]
filter' p (x : xs) | p x = x : filter' p xs
                   | otherwise = filter' p xs

filterF p = foldr f []
    where f x r | p x = x : r    
                | otherwise = r   

{-
concat' 
reverse
++
map
filfer
-}