--import Prelude hiding (min, elem) 

--Ejercicio 1
--(a)
--min::(Ord a) => a -> a -> a
--min x y = if x < y then x else y

--(b)
paren::(Show a) => a -> String
paren x = "(" ++ show x ++ ")"

--Ejercicio 2
data Semaforo = Verde | Amarillo | Rojo deriving (Show)

--Ejercicio 3
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) ys = merge xs (insert x ys)  

insert::(Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert a (x:xs) 
        |a <= x = a:x:xs 
        |otherwise = x: insert a xs

merge' :: (Ord a) => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] xs = xs
merge' (x:xs) (y:ys) | x < y = x:y:merge' xs ys
                    | otherwise = y:x:merge' xs ys        

--Ejercicio 4
--(a)
sumSqsRec :: (Num a) => [a] -> a
sumSqsRec [] = 0
sumSqsRec (x:xs) = x^2 + sumSqsRec xs

sumSqsR :: (Num a) => [a] -> a
sumSqsR = foldr sqs 0 
            where sqs x f = x * x + f

sumSqsL :: (Num a) => [a] -> a
sumSqsL = foldl sqs 0 
            where sqs f x = x * x + f


--(b)
{-elemRec :: (Eq a) => a -> [a] -> Bool
elemRec _ [] = False
elemRec = undefined
-}
elem :: (Eq a) => a -> [a] -> Bool
elem a = foldr pert False
            where pert x f
                    | a == x = True
                    | otherwise = f

             
--Ejercico 5
h x xs = x - sum xs

h1 x xs = foldr (-) x xs
h2 x xs = foldl (-) x xs

--La correcta es la b

--Ejercicio 6
split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xs) = (x:fst (split xs), y:snd (split xs))

splitv2 :: [a] -> ([a],[a])
splitv2 [] = ([],[])
splitv2 (x:xs) = case splitv2 xs of 
                    ([],[]) -> ([x],[])
                    (y:ys,[])  -> ([x], [y])
                    (ys,zs) -> (x:zs,ys)

splitFoldr :: [a] -> ([a],[a])
splitFoldr = foldr h ([],[])
            where h x r = case r of
                    ([],[]) -> ([x],[])
                    (y:ys,[])  -> ([x], [y])
                    (ys,zs) -> (x:zs,ys)


{-
p3SplitFoldr :: Num a => [a] -> ([a],[a])
p3SplitFoldr xs = foldr(dividir) ([],[]) xs
             where dividir x (ys,zs) | length ys == length zs = (x:ys,zs)
                                     | otherwise = (ys,x:zs)
                                     -}

data Expr =  Lit Int |  Add Expr Expr |  Sub Expr Expr deriving Show
foo f = f (Lit 1) (Lit 2)

data Tree = Empty | Branch Int Tree Tree
flatten:: Tree -> [Int]
flatten Empty = []
flatten (Branch x l r) = flatten l ++ [x] ++ flatten r


data Par a b = Par a b 