--Practico 3
--Si piden "usando" y la haces "como" no pasa nada
--cuando piden "como fold/fold, no se puede pre procesar la lista"
--f a b = foldr f e

--cuando piden "usando fold/fold", se pueden usar funciones auxiliares y eso


--Ejercicio 6
--la definimos como recursion explicita
split::[a] -> ([a],[a])
split = foldr h ([],[])
            where h x r = case r of
                    ([],[]) -> ([x],[]) -- esta linea tambien se puede sacar
                    ([y],[])  -> ([x], [y]) --otra forma (y:[],[])  -> ([x], [y]), igual este caso no es necesario
                    (ys,zs) -> (x:zs,ys)

--esta version tiene lio:
--no considera la lista vacia
--si la lista es de largo par, el primer elemento va a la 2da lista, y no a la primera 
splitA' (x:[]) = ([x],[]) 
splitA' (x:xs) = if length(fst p) > length(snd p) 
                then (fst p, x:snd p) 
                else (x:fst p, snd p)
                    where p = splitA' xs

--definimos slipt como folder, f combina la cabeza de la lista con la llamada recursiva
{-split_fr::[a] -> ([a],[a])
split_fr  foldr f e ([],[])
    where  
        f hd (ys,zs) = (hd:zs,ys)
-}

--otra version
split_fr::[a] -> ([a],[a])
split_fr = foldr (\hd (ys,zs) -> (hd:zs,ys)) ([],[])

--ahora usando fold
split_fl::[a] -> ([a],[a])
split_fl = rotar .
    foldl
    (\(xs,ys) a -> (ys ++ [a], xs)) 
    ([],[])
    where
        rotar (xs,ys)
            = if not (length ys < length xs)
              then (ys, xs)
              else (xs, ys)

--otra forma con fold, pasando el largo del acumulador como parametro
split_fl'::[a] -> ([a],[a])
split_fl' = fst .
    foldl f (([],[]), True)
    where
        f ((xs,ys), b) a 
            = if b then
                ((xs ++ [a], ys), not b)
              else ((xs, ys ++ [a]), not b)

  {- Solo para ver la dif entre folr y fold  
 fold f e [1,2,3] = 1 'f' (2 'f' (3 'f' e))
 foldl f e [1,2,3] ()=(e 'f' 1) 'f' 2 ) 'f' 3) 
 -}



 --Ejercicio 8
 --como foldr
takeWhile_fr :: (a -> Bool) -> [a] -> [a]
takeWhile_fr p = 
    foldr  f []
    where 
        f hd re =
            if p hd
            then hd : re
            else []

-- usando foldr
dropWhile_fr :: (a -> Bool) -> [a] -> [a]
dropWhile_fr p = fst . foldr f ([],[])
    where
        f hd (dwxs,idxs)
            = if p hd
              then (dwxs, hd:idxs)
              else (hd:idxs, hd:idxs)

dw ::(a -> Bool) -> [a] -> [a]
dw p [] = []
dw p (x:xs) = 
    if p then
        dw p xs
    else
        (x:xs)

