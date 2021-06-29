--Ejercicio 1
{-
call by value:
    reducir primero los argumentos

call by name:
    sustitucion

lazy evaluation = call by name + sharing, que lo mismo que call by need

si converge en call-by-value => converge en call-by-name


-}

-- (a)
--cal by value
     head (replicate 5 1)
>    head (1 : replicate (5 - 1) 1)
>    head (1 : replicate 4 1)
>    head (1 : 1 : replicate (4 -1) 1)
>    head (1 : 1 : replicate 3 1)
>x2  head (1 : 1 : 1 : replicate 2 1)
>x4  head (1 : 1 : 1 : 1 : 1 : replicate 0 1)
>    head (1 : 1 : 1 : 1 : 1 : [])     -- Notacion: 1 : 1 : 1 : 1 : [] == [1,1,1,1]
>    1

--lazy
  head (replicate 5 1)
> head (1 : replicate (5-1) 1)
> 1

-- (b) por valor (o greedy)
   map (*2) (replicate 2 2)
>  map (*2) (2 : replicate2 :  (2 - 1) 2)
>  map (*2) (2 : replicate2 :  1 2)
>  map (*2) (2 : 2 : replicate2 : (1 -1 ) 2)
>  map (*2) (2 : 2 : replicate2 : 0 2)
>  map (*2) (2 : 2 : replicate2 : [])
>  map (*2) (2 : 2 : [])
> (*2) 2 : map (*2) (2 : [])
> 4 : map (*2) (2 : [])
> 4 : (*2) 2 : map (*2) []
> 4 : 4 : map (*2) []
> 4 : 4 : []

-- lazy
   map (*2) (replicate 2 2)
>  map (*2) (2 : replicate2   (2 - 1) 2)
>  map (*2) (2 : replicate2   (2 - 1) 2)
>  (*2) 2 : map (*2) (replicate2 : (2 - 1) 2) --esto ya esta en weak head normal form, que es cuando hay un constructor (:) o un lambda
>  4 : map (*2) (replicate2   (2 - 1) 2)
>  4 : map (*2) (replicate2   1 2)
>  4 : map (*2) (2 : replicate2   (1 - 1) 2)
>  4 : (*2) 2 : map (*2) (2 : replicate2  (1 - 1) 2)
...

-- (c)
--lazy
    length (map (∗2) (replicate 2 2))
>*2 length ((*2) 2 : map (*2) (replicate2   (2 - 1) 2)))
>   1 + length (map (*2) (replicate2 :  (2 - 1) 2))
>*  1 + length ((*2) 2 : map (*2) (2 : replicate2  (1 - 1) 2))
>*  1 + (1 + length (replicate (1 - 1) 2))
>*  1 + (1 + length []) 

--Ejercicio extra con lazy
--length . take 2 (repeat 2)
-- (f . g) x = f (g x)

   (length . take 2) (repeat 2) --hay que aparecer un lambda del lado izq
>  (length (take 2 (repeat 2))
>  (length (take 2 (2 : repeat 2))
>  (length (2 : take 2 (2 - 1)  (repeat 2))
>  1 + length (take (2 - 1)  (repeat 2))
>  1 + length (take 1 (repeat 2))
>  1 + length (take 1 (2 : repeat 2))
>  1 + length (2 : (take (1 - 1) repeat 2))
>  1 + (1 + length ((take (1 - 1) (repeat 2))))
>  1 + (1 + length ((take 0 (repeat 2))))
>  1 + (1 + length [])
>  1 + (1 + 0)
>* 2

--Ejercicio 4
-- (a)
from n = n : from (n + 1)
nats = from 0

lista1 = map fst (zip nats (1 : lista1 ))

   lista1
>   map fst (zip nats (1 : lista1))
>*  map fst (zip (0 : from 1) (1 : lista1))
>   map fst ((0,1) : zip (from 1) (lista1))
>   fst (0,1) : map fst zip (from 1) (lista1))
>   fst (0,1) : map fst zip (from 1) (lista1))
>   0 : map fst zip (from 1) (lista1))
=   0 : map fst zip (1 : from 2) (0 : tail lista1))

    0 : 1 : 2 ...

--Ejercicio 6(
hamming = 1 : merge (xN 5) (merge (xN 3) (xN 2))
    where xN n = map (*) hamming
{-
hammingTo::Integer -> [Integer]
merge [] []= []
merge [] x = x
merge x [] = x
merge x@(a:as) y@(b:bs) | a<b       = a : (merge as y)
                        | b<a       = b : (merge x bs)
                        | otherwise = a : (merge as bs)
hammingTo 2 =[1]
hammingTo x =let h=hammingTo (x-1) in merge (merge h [2*i | i<-h,2*i<x]) (merge [3*i | i<-h,3*i<x] [5*i | i<-h,5*i<x])

-}
hamming = 1 : 
