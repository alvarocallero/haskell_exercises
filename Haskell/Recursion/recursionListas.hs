--RECURSION SOBRE LISTAS
------------------------

--Largo de una lista
length'::[a] -> Int
length' [] = 0
length' (x:xs) = 1 + length'(xs)

--La suma de todos los elementos de la lista
sum'::(Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum'(xs)

--Si todos los elementos de la lista cumplen con un predicado
all'::(a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = if p x == False then False else all' p xs 

all''::(a -> Bool) -> [a] -> Bool
all'' p [] = True
all'' p (x:xs) = p x && all'' p xs 

--La funcion map
map'::(a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x:map f xs

--La funcion filter
filter'::(a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) 
        | p x = x:filter' p xs
        | otherwise = filter' p xs

--Hacer el sorting de una lista
isort::(Ord a) => [a] -> [a]
isort [] = []
isort  (x:xs) = insert x (isort xs)

--usamos la funcion auxuliar que insertar un elemento de forma ordenada en una lista
insert::(Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert a (x:xs) 
        |a <= x = a:x:xs 
        |otherwise = x: insert a xs

--hacer la funcion init recursiva
init'::[a] -> [a]
init' [x] = []
init' (x:xs) = x : init' xs

--hacer la funcion last recursiva
last'::[a] -> a
last' [x] = x
last' (x:xs) = last' xs

--hacer la funcion zip recursiva
zip'::[a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--hacer la funcion zipWith vacia
zipWith'::(a -> b -> c) -> [a] -> [b] -> [c] 
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = x `f` y : zipWith' f (xs) (ys)

--hacer la funcion take recursiva
take'::Int -> [a] -> [a]
take' n _ | n <= 0 = [] 
take' _  [] = []
take' n (x:xs) = x : take' (n-1) xs

--hacer la funcion drop recursiva
drop'::Int -> [a] -> [a]
drop' n xs | n <= 0 = xs 
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

--hacer la funcion splitAt recursiva
esplitAt::Int -> [a] -> ([a],[a])
esplitAt n xs | n <= 0 = ([],xs)
esplitAt _ [] = ([],[])
esplitAt n (x:xs) = (x:l1,l2)
        where (l1,l2) = esplitAt(n-1) xs

--Algoritmo QuickSort
qsort::Ord a=> [a] -> [a]
qsort [] = []
qsort(x:xs) = qsort leq_x ++ [x] ++ qsort gt_x
        where leq_x = [y|y<-xs, y <= x]
              gt_x  = [y|y<-xs, y > x]

--hacer la funcion replicate de forma recursiva
replicate'::Int -> b -> [b]
replicate' n a
        | n <= 0 = []
        | otherwise = a : replicate' (n-1) a

--hacer la funcion reverse recursiva
reverse'::[a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--hacer la funcion elem recursiva
elem'::(Eq a ) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) 
            | a == x = True
            | otherwise = elem' a xs

-----------
-- foldr --
-----------

--Escribir la suma de los elementos de la lista como foldr
suma::(Num a) => [a] -> a
suma xs = foldr (+) 0 xs 

--Escribir la func del largo de lista como foldr
largoL::[a] -> Integer
largoL xs = foldr flen 0 xs
                where flen _ r = 1 + r

--Escribir la funcion all usando foldr, r es la llamada recursiva sobre el resto de la lista
allFoldr::(a -> Bool) -> [a] -> Bool
allFoldr p xs = foldr fall True xs
                where fall x r = p x && r

allFoldrv2 e = foldr (\x -> (&&) $ e==x) True


--map como foldr
--mapFoldr f xs = foldr ((:) . f) [] $ xs

--filter como foldr
--filter p xs = foldr fill [] xs
 --       where fill x r | p x = x : r
--                       | otherwise = r


--crear una funcion que devuelva la cantidad de ocurrencias de un elemento en una lista
--count e = foldr (\x acc -> if e == x then acc+1 else) 0
-----------
-- foldl --
-----------

--Suma acumulativa 
sumaAc::(Num a) => [a] -> a
sumaAc = sumacc 0
        where sumacc s [] = s
              sumacc s (x:xs) = sumacc (s + x) xs

--suma acumulativa escrita como foldl
sum xs = foldl (+) 0 xs