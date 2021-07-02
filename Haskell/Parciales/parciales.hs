
-----------------------------------------------------------------------------
------ 2020 -----------------------------------------------------------------
-----------------------------------------------------------------------------

--usando foldr

takeUntilR :: (a -> Bool) -> [a] -> [a]
takeUntilR p = foldr(\x acc -> if p x then [] else x : acc) []


takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x:xs) 
    | p x = []
    | otherwise = x : takeUntil p xs




data Tree a = Node (Tree a) a (Tree a) | Empty
mkTree n = Node (mkTree $ n + 1) n (mkTree $ n + 2)
goLeft (Node l a r) = a : goLeft l
goRight (Node l a r ) = a : goRight r
recorre t = go [t]
go (Node l v r : ts) = v : go (ts ++ [l] ++ [r])



suma xs = foldr (\x acc -> x:acc) [] xs

sumaL xs = foldl (\acc x -> x:acc) [] xs

main = do 
          foo <- putStrLn "Hello, what's your name?" 
          name <- getLine 
          putStrLn ("Hey " ++ name ++ ", you rock!")

-----------------------------------------------------------------------------
------ 2019 -----------------------------------------------------------------
-----------------------------------------------------------------------------

lookup' :: Eq k => k -> [(k, a)] -> Maybe a
lookup' k = foldl (\ acc x -> if fst x == k then (Just (snd x)) else acc) Nothing


foo [] = id
foo (x : xs) = foo xs . (+1)
bar xs = foo xs 0


-----------------------------------------------------------------------------
------ 2018 -----------------------------------------------------------------
-----------------------------------------------------------------------------
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s = foldr (\x (y:ys) -> if s == x then []:y:ys else (x:y):ys ) [[]]




-----------------------------------------------------------------------------
------ 2017 -----------------------------------------------------------------
-----------------------------------------------------------------------------
elimDups :: Eq a => [a] -> [a]
elimDups = foldr (\x acc -> if (null acc /= True && x == head acc) then acc else x : acc) []