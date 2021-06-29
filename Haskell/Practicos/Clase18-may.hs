--Practico 4

--Ejercicio 1

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

duplica :: Nat -> Nat
duplica Zero = Zero
duplica (Succ n) = Succ (Succ (duplica n))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


--Ejercicio 3
data Tree a = Empty | Node (Tree a) a (Tree a)

foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree  _ e Empty         = e
foldTree f e (Node l x r)  = f (foldTree  f e l) x (foldTree  f e r)

-- funcion que cuente nodos
cantNdos = foldTree (\nl _ nr -> 1 + nl + nr) 0

inorderF  = foldTree (\il x ir -> il ++ [x] ++ ir) []

inorderF'  = foldTree (\fl x fr -> \xs -> fl (x : fr xs)) id


inorder :: Tree a -> [a]
inroder Empty        = [] 
inorder (Node l x r) = inorder l ++ [x] ++ inorder r -- inroder l ++ x : inorder r esto es lo mismo

-- para que no sea de orden cuadratica el inorder
inorder2 t = inorder' t []

inorder' :: Tree a -> [a] -> [a]
inorder' Empty xs        = xs
inorder' (Node l x r) xs = inorder' l (x : inorder' r xs)
        

preorder :: Tree a -> [a]
preorder Empty        = [] 
preorder (Node l x r) = x : preorder l ++ preorder r  


-- Ejercicio 4
data BTree a = Leaf a | Fork (BTree a) (BTree a)

depths :: BTree a -> BTree Int
depths t = depthsAux t 0

depthsAux (Leaf x)   p = Leaf p
depthsAux (Fork l r) p = Fork (depthsAux l (p+1)) (depthsAux r (p+1)) 
--depthsAux (Fork l r) p = Fork (depthsAux l p') (depthsAux r p') 
--      where p' = p+1

--otra solucion de un pibe
depths'::BTree a->BTree Int
depths' ts = depths1 [0..] ts

depths1::[Int]->BTree a->BTree Int
depths1 (x:xs) (Leaf _) = Leaf x
depths1 (x:xs) (Fork izq der) = Fork (depths1 xs izq) (depths1 xs der)

-- ejercicio de un compañero
retrieve::BTree a->Int->a
retrieve ts n = (inordB ts)!!(n-1)

inordB::BTree a ->[a]
inordB (Leaf x) = [x]
inordB (Fork izq der) = inordB izq ++ inordB der

retrieve'::BTree a->Int->a
retrieve' (Leaf x) 1 = x 
retrieve' (Fork l r) n | s < n     = retrieve' r (n-s)
                       | otherwise = retrieve' l n
            where s = size l

size :: BTree a -> Int
size (Leaf _) = 1
size (Fork l r) = size l + size r
