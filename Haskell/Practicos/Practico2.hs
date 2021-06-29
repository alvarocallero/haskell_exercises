import Prelude hiding (map, length, all, elem, flip) 


--Ejercicio 1
--(a)
map::( a -> b ) -> [a] ->[b]
map f xs = [f $ x | x <- xs]

--(b)
filter'::(a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (y:ys) = if p y then 
                      [y] ++ filter' p ys 
                   else  
                      filter' p ys
                    

--Ejercicio 2
squares::[Int] -> [Int]
squares xs = map (^2) xs


--Ejercicio 3
--length::[t] -> Int
length xs = sum( map (\_ ->1) xs)

length' xs = sum . map (\_ ->1) $ xs

length'' xs = (sum . map (\_ ->1)) xs




--Ejercicio 4
--(a)
all :: (a -> Bool) -> [a] -> Bool
all p xs = length (filter p xs) == length xs

--(b)
elem::Eq a => a -> [a] -> Bool
elem a xs = length (filter (==a) xs) > 0


--Ejercicio 5
--Primero vemos como es el tipo de la composicion de funciones
{-(f . g) x = f (g x)
(.)::(a -> b) -> (c -> d) -> ( d-> e)
         f          g          f.g
A su vez, si consideramos que la salida de g es la entrada de f, tenemos que:
(.)::(a -> b) -> (c -> a) -> ( d-> e)

Luego la entrada de f . g es la entrada de g, y la salida de f . g es la salida de f: 
(.)::(a -> b) -> (c -> a) -> ( c-> b)

Entonces rara p es:
rara p = filter p . filter (not . p)
         [a] -> [a] [a] -> [a]

por lo que finalmente tenemos que:
rara::(a -> Bool) -> [a] -> [a]
-}

--Ejercicio 6
dup x = x ++ x

--(dup . dup) es la composicion de la funcion dup con ella misma, por lo que (dup . dup) devuelve como resultado una funcion
--(dup dup) no es composicion, si no que es una aplicacion de dup 2 veces


--Ejercicio 7
--zipWith::(a -> b -> c) -> [a] -> [b] -> [c]

--rara2 = zipWith . [length,sum][drop 4,take 4]



--Ejercicio 8
twice f = f . f

--Al hacer twice tail [1,2,3,4] = (tail . tail) [1,2,3,4] = tail(tail([1,2,3,4])) = tail([2,3,4]) = [3,4]
--Al hacer twice head [1,2,3,4] = (head . head) [1,2,3,4] = head(head([1,2,3,4])) = head (1)



--Ejercicio 9
--flip toma una funcion como parametro y 2 entradas, y evalua la funcion intercambiando el orden de las entradas
flip::(a -> b -> c) -> b -> a -> c
flip f a b = f b a


--Ejercicio 10 - No se me ocurre como usar flip aca
--(a)
--cantPares::[Integer] -> [Integer]

--(c)
cantPares xs = length . filter (==0) . map (\x -> if x `mod` 2 == 0 then 0 else 1) $ xs



--Ejercicio 13
data Triangulo = Equi Int | Iso Int Int | Esca Int Int Int deriving (Show, Eq)

lst =[Equi 1, Iso 2 3, Esca 1 2 3, Iso 22 23, Iso 23 53]
--(a)
esIso::Triangulo -> Bool
esIso (Iso _ _) = True
esIso _ = False

isos :: [Triangulo] -> Int
isos xs = length [1|x<-xs, esIso x]

--(b)
isos' :: [Triangulo] -> Int
isos' xs = length . filter (esIso) $ xs

k= []