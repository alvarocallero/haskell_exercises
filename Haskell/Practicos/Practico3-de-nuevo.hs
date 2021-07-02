--Ejercicio 1
--a) Ord a => a -> a -> a
--b) Show a => a -> String 

min' x y = if x < y then x else y
paren x = "(" ++ show x ++ ")"


--Ejercicio 3
merge :: Ord a => [a] -> [a] -> [a]
merge [] []     = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) ys = merge xs (insOrd x ys)
    --insOrd (x ys) ++ merge xs ys

insOrd :: Ord a => a -> [a] -> [a]
insOrd a [] = [a]
insOrd a (x:xs) 
        | a < x = a : x : xs
        | otherwise = x : insOrd a xs


--Ejercicio 4
--a)
sumSqs :: Num a => [a] -> a
sumSqs = foldr (\ x acc -> acc + x^2) 0

--b)
elem' :: Eq a => a -> [a] -> Bool
elem' a = foldl (\acc x -> a == x && True) False

--c)
elimDups :: Eq a =>[a ] -> [a ]
elimDups = foldr (\ x acc -> if (null acc == False && head(acc) == x) then acc else x : acc ) []

--elim dup sin foldr
elimD :: Eq a =>[a ] -> [a ]
elimD [] = []
elimD [a] = [a]
elimD (x:y:ys)
    | x == y = elimD (y:ys)
    | otherwise = x : elimD (y:ys)


--Ejercicio 6
split :: [a] -> ([a], [a])
split [] = ([],[])
split [a] = ([a],[])
split (x:y:ys) = ([x] ++ fst(split ys),[y] ++ snd(split ys))

--a
splitR::[a] -> ([a],[a])
splitR = foldr(\x (xs,ys) -> (x:ys,xs)) ([],[])    

--split_fr = foldr (\hd (ys,zs) -> (hd:zs,ys)) ([],[])


{-splitR xs = foldr func ([],[]) xs
    where func x acc | even . length $ xs = x : fst(acc)
                     | otherwise           = x : snd(acc) 
-}

--Ejercicio 7
{-
maxInd :: Ord a => [a] -> (a,Int)
maxInd xs = foldr (\ x (num,ind) -> if x == maximum xs then (x,ind) else (num,ind+1)) (0,0) xs
-}

--Ejercicio 8
takeWhileR :: (a -> Bool) -> [a] -> [a]
takeWhileR p = foldr (\ x acc -> if p x then x:acc else [] ) []

takeWhileR' :: (a -> Bool) -> [a] -> [a]
takeWhileR' p = foldr f []
    where f x acc | p x       = x : acc
                  | otherwise = acc