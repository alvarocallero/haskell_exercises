--Ejercicio 1

--(a)
zero = Zero
uno = Succ Zero
dos = Succ (Succ Zero)
tres = Succ (Succ (Succ Zero))
cuatro = Succ (tres)
cinco = Succ (cuatro)
seis = Succ (cinco)
siete = Succ (seis)


duplica :: Nat -> Nat
duplica Zero = Zero
duplica (Succ n) = Succ (Succ (duplica n))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

suma :: Nat -> Nat -> Nat
suma Zero n = n
suma n Zero = n
suma (Succ a) (Succ b) = Succ ( Succ (suma a b))

predecesor :: Nat -> Nat
predecesor Zero = Zero
predecesor (Succ n) = n

--(b)
foldN :: (a -> a) -> a -> Nat -> a
foldN h e Zero = e
foldN h e (Succ n) = h (foldN h e n)

nat2intFoldN:: Nat -> Int
nat2intFoldN n = foldN (+1) 0 n 

duplicaFoldN:: Nat -> Nat
duplicaFoldN n = foldN suc Zero n
        where suc r = Succ (Succ r)

duplicaFoldN2 :: Nat -> Nat
duplicaFoldN2 n = foldN (\x -> (Succ (Succ(x)))) Zero n


sumaFoldN:: Nat -> Nat -> Nat
sumaFoldN n1 n2 = foldN suma (foldN suma Zero n2) n1
        where suma r = Succ r

sumaFoldN2 :: Nat -> Nat -> Nat
sumaFoldN2 n1 n2 = foldN (\x -> Succ x) (foldN (\y -> Succ y) Zero n2) n1

-- (c)
fib :: Nat -> Nat
fib Zero = Zero
fib (Succ Zero) = (Succ Zero)
fib (Succ n) = suma (fib n)(fib (predecesor (n)))


--Ejercicio 2
-- (a)
{-
class Num a where
    (+), (-), (*)  :: a -> a -> a
    negate         :: a -> a
    abs            :: a -> a
    signum         :: a -> a
    fromInteger    :: Integer -> a
-}
data Nat = Zero | Succ Nat deriving Show
data OurInt = IntZero | Pos Nat | Neg Nat deriving Show

int0 = IntZero
int1 = Succ (Zero)

--instance Num OurInt where
--    IntZero + IntZero = IntZero

-- (b)
data OtroInt = OZero | OPos OtroInt | ONeg OtroInt
zero' = OZero
--El problema es que tenes muchas formas de definir el mismo numero

data OtroInt' = OPos' Nat | ONeg' Nat
--Esta representacion tampoco tiene para obtener el sucesor y el predecesor
--y tampoco tiene la representacion del cero



--Ejercicio 3
data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show
arbol = Node (Node Empty 2 Empty) 3 (Node Empty 4 Empty)
-- (a)
-- raiz izq der
preOrder::Tree a -> [a]
preOrder Empty           = []
preOrder (Node l a r)    = [a] ++ preOrder l ++ preOrder r  

-- izq raiz der
inOrder::Tree a -> [a]
inOrder Empty           = []
inOrder (Node l a r)    =  inOrder l ++ [a] ++ inOrder r  

-- izq der raiz
postOrder::Tree a -> [a]
postOrder Empty           = []
postOrder (Node l a r)    =  postOrder l ++ postOrder r ++ [a] 

-- (b)
insord::Ord a => a -> Tree a -> Tree a
insord x Empty = Node Empty x Empty
insord x (Node l a r)
            | x > a = Node l a (insord x r)     
            | x < a = Node (insord x l) a r
            | otherwise = Node l a r

mkTree :: Ord a => [a] -> Tree a
mkTree [] = Empty
mkTree (x:xs) = insord x $ mkTree xs

-- (c)

-- Ejercicio 4
-- (a)
arbol4 = (Fork (Fork (Leaf 'a') (Leaf 'b')) (Leaf 'c')) 

depthsAux::BTree a -> Int -> BTree Int
depthsAux (Leaf x)   p = Leaf p
depthsAux (Fork l r) p = Fork (depthsAux l (p+1)) (depthsAux r (p+1)) 

depths :: BTree a -> BTree Int
depths t = depthsAux t 0

--(b)
-- Dado un arbol, devuelve la cantidad de hojas
sizeT :: BTree a -> Int
sizeT (Leaf a) = 1
sizeT (Fork l r) = sizeT (l) + sizeT (r)  

balanced :: BTree a -> Bool
balanced (Leaf a) = True
balanced (Fork l r) = abs (sizeT l - sizeT r) <= 1

-- (c)
splitList::[a] -> ([a],[a])
splitList [] = ([],[])
splitList xs = splitAt ((length (xs)) `div` 2) xs

--inserta en la rama izquierda
insertLeft::a -> BTree a -> BTree a
insertLeft a (Leaf x) = Fork (Leaf a)(Leaf x)
insertLeft a (Fork l r) = Fork (insertLeft a l)(r)

--inserta en la rama derecha
insertRight::a -> BTree a -> BTree a
insertRight a (Leaf x) = Fork (Leaf a)(Leaf x)
insertRight a (Fork l r) = Fork (l)(insertRight a r)

--mkBTree :: [a] -> BTree a
--mkBTree xs = Fork(fst (splitList xs)) (snd (splitList xs)) 

-- (d)


inOrder4::BTree a -> [a]
inOrder4 (Leaf a)    = [a]
inOrder4 (Fork l r)  =  inOrder4 l ++ inOrder4 r 

retrieveAux::BTree a -> Int -> BTree Int
retrieveAux (Leaf x)   p = Leaf (p + 1)
retrieveAux (Fork l r) p = Fork (retrieveAux l (p)) (retrieveAux r (p)) 



--Ejercicio 5
data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving (Show, Eq)

data HTree a = Tip a | Bin (HTree a) a (HTree a)

--mapHT :: (a -> b) -> (HTree a -> HTree b)
--mapHT f (Tip a) = (Tip a)



--Ejercicio 6
-- (a)
class Sizeable a where
        size::a -> Int

instance Sizeable Int where
        size a = abs a

instance Sizeable Char where
        size _ = 1

-- (b)
instance Sizeable [a] where
        size [a] = sum [1|_<-[a]]

-- (c)
{-
instance Sizeable (Int a => Tree a) where
        size Empty = 0
        size (Node l a r) = size l + a + size r 
-}
-- (d)
--filterSize::(Sizeable a, Int b) => [a] -> b -> [a]
--filterSize = undefined

