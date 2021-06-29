--Suma acumulativa
sumA :: Num a => [a] -> a
sumA = sumacc 0
    where sumacc ac [] = ac
          sumacc ac (x:xs) = sumacc (ac + x) xs

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\ acc x -> acc + x) 0 xs

--Como la suma esta currificada, se puede reescribir asi:
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs

--Implementar el elem con foldl
elem' ::Eq a => a -> [a] -> Bool
elem' a xs = foldl (\ acc x -> if a == x then True else acc) False xs

--Implementar el elem como foldr
elem'' ::Eq a => a -> [a] -> Bool
elem'' a xs = foldr (\x acc -> if a == x then True else acc) False xs

--map :: a -> b -> [a] -> [b]
--foldr :: (a -> b -> b) -> b -> [a] -> b
--Implementar un map con foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc ) [] xs

--filter con foldl
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldl(\ acc x -> if p x then x : acc else acc) []

--filter con foldr
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr(\x acc -> if p x then acc ++ [x] else acc) []

--hacer el reverse con fold
reverse' :: [a] -> [a]
reverse' = foldl(\acc x -> x : acc ) []

--lo anterior se puede hacer usando flip
reverseConFlip' :: [a] -> [a]
reverseConFlip' = foldl(flip(:)) []
