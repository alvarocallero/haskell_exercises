-- Para ocultar funciones del Prelude
--import Prelude hiding (length,all)

--Practico 2
--Ejercicio 3
length' xs = sum (map to1 xs)
to1 x = 1

--otra manera
length'' xs = sum (map to1' xs)
to1' x = 1

--otra manera
length''' xs = sum (map (\_ -> 1) xs)

--otra manera
--usamos la funcion const x que siempre devuelve x
length'''' xs = sum (map (const 1) xs)

--otra forma
length''''' xs = sum . map (const 1)


--Ejercicio 4
--(a)

all'::(a-> Bool) -> [a] -> Bool
all' p xs = length (filter p xs) == length xs

--otra forma
all''::Eq a => (a-> Bool) -> [a] -> Bool
all'' p xs = filter p xs == xs

--otra forma
all'''::Eq a => (a-> Bool) -> [a] -> Bool
all''' p xs = null (filter (not . p) xs) 

--otra forma
all''''::Eq a => (a-> Bool) -> [a] -> Bool
all'''' p = null . filter (not . p) 

--(b)
elem':: Eq a => a -> [a] -> Bool
elem' x xs = length (filter (==x) xs) >= 1

--otra forma
elem'':: Eq a => a -> [a] -> Bool
elem'' x  = not . null . filter (==x)


--Ejercicio 5
rara p = filter p . filter (not . p)

--se pued ereescribir como
rara' p a = filter p ( filter (not . p) a)

--Esto se cumple siempre:
filter p . filter q = filter r
donde r x = p x && q x

rara p = filter p . filter (not . p)
         [a]->[a]     [a]->[a]  
filter::(a->Bool) ->[a]->[a]
rara::(a-> Bool)->

en definitiva:
rara::(a->Bool)->[a]->[a]