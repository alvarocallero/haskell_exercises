--Ejercicio 5
-- loop :: [a]
loop = tail loop --no produce nada
a = 1 : b
b = 2 : a


-- (a)
--[3,3,3,3]

--(b)
--cualquiera de las 2 condiciones diverge

--(c)
--12

--(d)
--8

--(e)
--head necesita un cons para devolver algo, pero (tail loop) nunca genera el cons

--Ejercicio 6
-- el merge es asociativo y conmutativo
hamming :: [Integer]
hamming = 1 : merge (xN 5 hamming) (merge (xN 2 hamming) (xN 3 hamming))
    where xN n xs = map (*n) xs

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x == y = x    : merge xs ys
                    | x < y  = x    : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--(b)
hammingTo n = takeWhile (<n) hamming

-- Ejericio 7
-- (a)
data Tareas = Limpiar | Cocinar | Fregar | Lavar | Comprar deriving (Show, Eq)

--(b)
tareasDeHogar = Limpiar : Cocinar : Fregar : Lavar : Comprar : tareasDeHogar

-- (c)
tareasPareja::Int -> [Tareas ] -> ([Tareas ], [Tareas ], [Tareas ])


--la de un compañero
tareasPareja n tasks = tareasParejaAux ([],[],[]) n tasks
                    where
                        tareasParejaAux (xs,ys,_) 0 ts = (xs,ys,ts) 
                        tareasParejaAux (xs,ys,_) 1 (x:ts) = (xs ++ [x],ys,ts) 
                        tareasParejaAux (xs,ys,_) n (x:y:ts) = tareasParejaAux (xs ++ [x],ys ++ [y],[]) (n-2) ts


tareasPareja' 0 ts         = ([], [], ts)
tareasPareja' 1 (t:ts)     = ([t], [], ts)
tareasPareja' n (t1:t2:ts) =
    let (ts1,ts2,ts') = tareasPareja' (n-2) ts
    in (t1:ts1, t2:ts2,ts')


